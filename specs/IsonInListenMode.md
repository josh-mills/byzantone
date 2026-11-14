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
- Recommend headphones (UI hint near the ison control in Listen mode). *Not yet
  done.*
- Request the mic with `echoCancellation`, `noiseSuppression`, and
  `autoGainControl` all `false` in `pitch_tracker.ts` `init()`. *Done.* This was
  originally planned as "echo cancellation on", but testing showed the opposite
  is needed (see Findings).

**Findings (`sandbox/ison_click.html`, MacBook speakers):**

| Browser | Clicks in the ison when the mic opens/closes |
| --- | --- |
| Chrome | Only with default constraints (`audio: true`), including when the mic is fed into the ison's own context. None with echo cancellation / noise suppression / AGC off, and none from an extra `AudioContext` alone. |
| Safari | Once, on the first open with default constraints. Otherwise clean. |
| Firefox | With every mic scenario, including processing off. None from an extra `AudioContext` alone. |

Consequences:
- Disabling mic processing fixes Chrome. It also gives the pitch detector an
  unprocessed signal.
- Firefox clicks on any mic open/close regardless of constraints, so avoiding it
  requires avoiding mic churn or masking it (see Open Questions).
- With echo cancellation off, the browser no longer attenuates the ison at the
  mic, so Phase 2 (in-worklet ison cancellation) becomes more important for
  speaker use.

**Phase 2, implemented after testing showed ison bleed on MacBook speakers
(wrongly detecting the ison during silence, and disturbing a fifth above it;
no problem on AirPods):**
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
- **Opt-in:** the canceller is controlled by `AudioSettings.filterIsonFromMic`
  (default `False`), shown in the Audio Settings menu in Listen mode as a
  "Speakers" fieldset with an "I'm listening through speakers" checkbox
  (helper text: "Keeps the drone from being mistaken for your voice. Leave off
  when using headphones."). The wording is deliberately user-facing rather than
  describing the filter mechanism. Elm only emits the `ison` attribute on `<pitch-tracker>`
  when the setting is on, so no JS changes were needed: with no `ison`
  attribute the worklet passes the mic signal through unchanged. Rationale:
  headphone users don't need it and shouldn't pay its costs (below). The
  setting is not persisted (neither are the other audio settings).
- Implementation notes:
  - The worklet source lives in `pitch_tracker.ts` as a string loaded from a
    Blob URL, so no separate worklet file needs building, hashing
    (`scripts/applyHash.mjs`), or serving. If `AudioWorklet` is unavailable or
    fails to load, the tracker falls back to analyzing the mic directly.
  - Elm passes the ison via the same `playbackFrequencyString` used for
    `<chant-engine>`, so both sides use identical frequencies.
  - Tunables are in `ISON_CANCELLER_CONFIG`: steady-state notch width
    (`bandwidthHz`, 1 Hz), a wider notch for `acquireSeconds` after the ison
    changes, and `harmonics` (1 = fundamental only; raise to 2 if small
    speakers radiate a distorted 2nd harmonic that still bleeds through).
  - `sandbox/canceller_check.mjs` runs the worklet offline against synthetic
    signals (`npx tsc && node sandbox/canceller_check.mjs`). Results at 48 kHz:
    bleed alone is removed to ~0 within ~0.5 s; a voice a fifth above the ison
    (330 Hz vs 220 Hz) is preserved at full amplitude with the ison residual
    ~0; a voice 3 Hz sharp of the ison is preserved (~99%); a retuned ison is
    re-acquired within ~0.1 s; with no ison the signal passes through
    unchanged.
  - Known cost: a voice within about ±1 Hz of the ison (at unison and nearly
    perfectly in tune) is partially cancelled, and a voice with wide vibrato
    centred on the ison loses roughly 40% of its amplitude (synthetic test).
    The spectrum visualization also shows a notch at the ison, since it reads
    from the post-cancellation signal.
- A simpler stopgap would have been a `BiquadFilterNode` notch at the ison
  frequency, which unconditionally removes the singer's fundamental at unison.
  The adaptive canceller is narrower.

**Phase 3, optional:** extract a shared `AudioContext` module used by both
elements (one clock, fewer system resources).

## Performance

Measured offline with `sandbox/perf_check.mjs` (`npx tsc && node sandbox/perf_check.mjs`;
node/V8 on a development machine, so use the ratios, not absolute numbers):

| Work | Cost |
| --- | --- |
| Worklet, no ison (passthrough, i.e. filter off) | ~0.03 ms per second of audio (negligible) |
| Worklet, filter on, 1 harmonic | ~5 ms per second of audio (~0.5% of a core); ~14 µs per 2667 µs render quantum |
| Worklet, 2 / 3 harmonics | ~11 / ~16 ms per second of audio |
| `autoCorrelate` (pre-existing), 2048-sample window | ~2.3 ms per call; at 60 fps ~140 ms per second (~14% of a core, ~14% of each frame), on the main thread |

The canceller is ~25x cheaper than the pitch detector it feeds, so it is not a
concern even at a 5-10x slower phone CPU. The pre-existing `autoCorrelate`
(O(N²) per animation frame, plus per-frame array allocations) is the main
CPU cost in Listen mode and the likelier problem on weak phones (more so on
120 Hz displays). Possible optimizations if needed: decimate the signal before
autocorrelating, limit the lag range to the expected pitch range, or run
detection at a lower rate (note the smoothing counters are frame-based).

**Follow-up to explore: downsampling in `autoCorrelate`.** Decimating the mic
signal (e.g. by 4, after a simple low-pass/averaging step) before
autocorrelating would cut the O(N²) work roughly 16x. Voice fundamentals are
well under 1 kHz, so the detectable range should be unaffected. To explore:
- Verify accuracy against the current implementation using the offline harness
  (synthetic voice-like signals across the full register range, plus recorded
  singing if available), paying attention to the sub-cent precision that the
  parabolic interpolation currently provides (it operates on lag, which becomes
  coarser after decimation, so interpolation matters more).
- Check the lowest (Bass register) frequencies still get at least two periods
  in the analysis window.
- Re-measure with `sandbox/perf_check.mjs` and, ideally, on a real low-end
  phone.
- Consider combining with a bounded lag range for further savings.

In Listen mode there is now also a second, always-mounted `AudioContext`
(`chant-engine`) even when no tone is sounding; suspending it while idle is a
possible follow-up (not measured).

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

- **Mic bleed on speakers**: mitigated by the Phase 2 canceller; needs
  on-device confirmation in Chrome, Safari, and Firefox (see §6).
- **Residual clicks on mic open/close in Firefox, and once in Safari.** Options:
  keep the mic stream open and pause analysis instead of tearing it down (the
  browser's mic-in-use indicator then stays on in Play mode; could tear down
  after an idle timeout), or briefly duck the ison around the transition.
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
