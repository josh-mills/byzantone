# Ison in Listen Mode

Status: Steps 1-3 implemented (View, Update, `chant_engine.ts`); manual browser testing and Phase 1 mic tweaks pending

## Goal

Allow the ison drone to sound while the app is in **Listen** mode, so a singer
can sing against the ison while the pitch tracker analyzes their voice.

## Non-goals

- Playing the melos (`Play`-mode pitch) in Listen mode.
- A new `AudioMode` variant. The ison is already independent state in
  `PitchState.ison`; it should stay orthogonal to `AudioSettings.audioMode`.
- Sharing a single `AudioContext` between `chant-engine` and `pitch-tracker`
  (optional future refactor; see Notes).

## Background

The Web Audio API has no notion of modes: an output graph (oscillator →
destination) and an input graph (mic → analyser) can run at the same time. The
current exclusivity is app-level:

| Where | Current behavior |
| --- | --- |
| `View.view` | `<chant-engine>` is mounted only when `audioMode == Play` (`viewIfLazy`). |
| `View.pitchTracker` | `<pitch-tracker>` is mounted only when `audioMode == Listen`. |
| `Update.SetAudioMode` | Resets `pitchState` to `initialPitchState`, clearing the ison. |
| `Update.processPitchButtonClick` | Listen branch only handles accidentals; it ignores `SelectingIson`. |
| `ChantEngine.disconnectedCallback` | Closes its `AudioContext`, so an unmounted engine can't be reused. |

`chant_engine.ts` already treats the `ison` and `melos` attributes
independently, so the engine needs only small changes.

Browsers support two simultaneous `AudioContext`s (Chrome's old limit of 6 was
removed in v66; WebKit's limit of 4 applies only on Windows). `ChantEngine` and
`PitchTracker` each own one and both close theirs on disconnect.

## Design

### 1. Keep `<chant-engine>` mounted in both modes (`View.elm`)

- Remove the `Play`-only `viewIfLazy` around `chantEngineNode`.
- Compute the melos pitch by mode:
  - `Play` → `PitchState.currentPitch scale pitchState`
  - `Listen` → `Nothing` (no `melos` attribute is emitted, so the engine stops
    any melos tone via `handleFrequencyChange`).
- The ison is passed in both modes.
- Keep the node's position in the view tree stable so Elm's virtual DOM never
  unmounts and re-creates it on mode switch.

### 2. Preserve the ison across mode changes (`Update.elm`)

`SetAudioMode` currently sets `pitchState = initialPitchState`. Change it to
reset everything *except* the ison:

Implemented as `PitchState.resetKeepingIson`:

- `Selected degree` → preserved.
- `SelectingIson (Just degree)` → `Selected degree` (the current ison keeps
  sounding).
- `SelectingIson Nothing` and `NoIson` → `NoIson`.
- `currentDegree`, `appliedAccidentals`, `proposedAccidental`, and
  `proposedMovement` still reset, as today.

### 3. Allow ison selection in Listen mode (`Update.elm`)

In `processPitchButtonClick`'s `Listen` branch, add a case before the
accidental handling: if `pitchState.ison` is `SelectingIson _`, set
`ison = Selected degree` and clear any accidental on that degree (mirroring the
Play-mode behavior, since the ison is always a natural pitch). Otherwise keep
the existing accidental-only behavior.

The ison menu (`View.Controls` `IsonMenu`), the `SetIson` message, and the `i`
keyboard shortcut (`handleIsonToggle`) are not mode-gated, so they should
already work. Verify `PitchSpace` ison indicators render correctly in Listen
mode.

Update the doc comment on `processPitchButtonClick`, which currently says
Listen mode is "only for setting the accidental".

### 4. Ison frequency and register

Keep using `audioSettings.playbackRegister` for the ison frequency in both
modes. `listenRegister` can be `Auto` and flip between Bass and Treble based on
the detected pitch, which would make the ison jump octaves unexpectedly.

Extract the pitch → frequency calculation from `chantEngineNode` into a
shared helper, because the pitch tracker may also need the ison frequency
(Phase 2).

### 5. `chant_engine.ts` robustness

- **Resume on demand.** The context is created in the constructor and never
  resumed. If the element is mounted before any user gesture (now possible,
  since it is always mounted), the context starts `suspended`. In
  `playTone`, call `audioContext.resume()` when `state === "suspended"`.
  Attribute changes originate from user input, so this runs in a gesture.
- **Guard construction.** Wrap `new AudioContext()` so a `NotSupportedError`
  is logged (via `devLog`/`console.error`) and the engine becomes a no-op
  instead of throwing from the constructor.
- **Optional: separate ison gain.** If a drone at `gain` (default 0.3) is too
  loud next to the singer or bleeds too much into the mic, add an `ison-gain`
  attribute. Defer unless testing shows a need.

### 6. Microphone bleed (the hard part)

The pitch tracker's `autoCorrelate` finds the strongest periodicity. A steady
sine from the speakers will dominate or contaminate detection, especially when
the singer is at or near the ison. Mitigation is phased.

**Phase 1, baseline (ships with the feature):**
- Recommend headphones (UI hint near the ison control in Listen mode).
- Set explicit `getUserMedia` constraints in `pitch_tracker.ts` `init()`
  instead of `{ audio: true }`, for example
  `{ audio: { echoCancellation: true, noiseSuppression: true, autoGainControl: false } }`.
  Evaluate empirically; browser behavior varies and noise suppression can
  distort voice.

**Phase 2, only if Phase 1 is insufficient on speakers:**
- Elm passes the ison frequency to `<pitch-tracker>` as an `ison` attribute
  (Hz, omitted when there is no ison).
- In `pitch_tracker.ts`, insert an `AudioWorklet` between the mic source and
  the analyser that removes the ison component using a two-weight LMS
  canceller:

  ```
  s = sin(2πft), c = cos(2πft)   // generated in-worklet from the ison frequency
  y = w1*s + w2*c                // estimate of the ison as heard at the mic
  e = x - y                      // output → analyser
  w1 += mu*e*s;  w2 += mu*e*c
  ```

  It needs only the frequency, not the playback signal, so it works across
  separate `AudioContext`s. `mu` controls cancellation bandwidth versus
  convergence time; tune so a singer at unison with natural vibrato is mostly
  preserved.
- A simpler stopgap is a `BiquadFilterNode` notch at the ison frequency, with
  the known drawback that it removes the singer's fundamental at unison and
  octaves.

**Phase 3, optional:** extract a shared `AudioContext` module used by both
elements (one clock, fewer system resources).

## Implementation Order

1. `View.elm`: always mount `chant-engine`; melos only in `Play`.
2. `Update.elm`: preserve ison in `SetAudioMode`; Listen-mode ison selection;
   doc comment update.
3. `chant_engine.ts`: resume-on-demand and constructor guard.
4. Manual testing (below); decide on Phase 1 tweaks.
5. Phase 2 only if bleed makes detection unusable on speakers.

## Acceptance Criteria

- Selecting an ison in Play mode and switching to Listen keeps the ison
  sounding without interruption (no click, no restart).
- Switching Listen → Play keeps the ison; the melos behaves as before.
- In Listen mode, the ison can be set via the Ison menu, the `i` shortcut, and
  pitch buttons while `SelectingIson`; tapping a pitch button otherwise still
  applies accidentals only.
- Clearing the ison in Listen mode silences it with the existing cross-fade.
- No melos tone ever sounds in Listen mode.
- Opening the app directly in Listen mode and selecting an ison produces sound
  (context resumed) in Chrome and Safari.
- With headphones, pitch detection accuracy in Listen mode with the ison
  playing is not noticeably worse than without it.

## Test Plan

Automated: `tests/Tests.elm` covers `PitchState.resetKeepingIson`. The
Listen-branch ison selection in `processPitchButtonClick` is not unit-tested
(it is private to `Update`); verify it manually.

- Browsers: Chrome and Firefox on desktop, Safari on macOS and on a physical
  iPhone. Watch the console for `NotSupportedError` or autoplay warnings.
- Scenarios: headphones and laptop speakers; singer at, near, and an octave
  from the ison; ison changed while the detector is running.


## Risks and Open Questions

- **Mic bleed on speakers** may make Phase 1 insufficient (see §6).
- **iOS audio session.** With the mic active, iOS uses a play-and-record
  session; ison playback may be quieter or route to the earpiece. Not fixable
  from Web Audio; needs on-device testing.
- **Keyboard shortcuts.** Confirm other `handleKeydown` keys (movement, etc.)
  behave sensibly in Listen mode now that the engine is live. They are not
  expected to affect the ison.
- **Ison register choice** (`playbackRegister` vs. a dedicated setting) may be
  worth revisiting after user testing.
- **`disconnectedCallback`** closes the context irreversibly. Fine while the
  element stays mounted; if it is ever conditionally mounted again, the engine
  will need to create its context lazily.
