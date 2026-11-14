# Ison in Listen Mode

## Status

Implemented: the ison sounds in both Play and Listen modes, the mic is
requested without browser processing, and an opt-in "Speakers" setting removes
the ison from the microphone signal before pitch detection.

Verified so far (Chrome, MacBook): no click in the ison when switching modes;
the speakers setting stops the ison bleeding into detection. Not yet verified:
Safari, Firefox, iPhone. See [Remaining Work](#remaining-work).

## Goal

Allow the ison drone to sound while the app is in **Listen** mode, so a singer
can sing against the ison while the pitch tracker analyzes their voice.

## Non-goals

- Playing the melos (`Play`-mode pitch) in Listen mode.
- A new `AudioMode` variant. The ison is independent state in
  `PitchState.ison` and stays orthogonal to `AudioSettings.audioMode`.
- Sharing one `AudioContext` between `chant-engine` and `pitch-tracker`
  (possible future refactor; see [Remaining Work](#remaining-work)).

## Background

The Web Audio API has no notion of modes: an output graph (oscillator →
destination) and an input graph (mic → analyser) can run at the same time. The
previous exclusivity was app-level:

| Where | Previous behavior |
| --- | --- |
| `View.view` | `<chant-engine>` mounted only in Play mode. |
| `View.pitchTracker` | `<pitch-tracker>` mounted only in Listen mode. |
| `Update.SetAudioMode` | Reset `pitchState` entirely, clearing the ison. |
| `Update.processPitchButtonClick` | Listen branch only handled accidentals. |
| `ChantEngine.disconnectedCallback` | Closed its `AudioContext`, so an unmounted engine couldn't be reused. |

`chant_engine.ts` already treated the `ison` and `melos` attributes
independently, so the engine needed only small changes.

Two simultaneous `AudioContext`s (engine and tracker) are fine: Chrome's old
limit of 6 was removed in v66, and WebKit's limit of 4 applies only on Windows.
Both elements close their context when disconnected.

## Design

### 1. `<chant-engine>` stays mounted in both modes (`View.elm`)

- The engine is no longer wrapped in a Play-only `viewIfLazy`; its position in
  the view tree is stable so it is never unmounted on a mode switch.
- The melos frequency is `""` in Listen mode, so no `melos` attribute is
  emitted and the engine stops any melos tone. The ison is passed in both
  modes.

### 2. The ison survives mode changes (`PitchState.resetKeepingIson`)

`SetAudioMode` resets pitch state except for the ison:

- `Selected degree` → preserved.
- `SelectingIson (Just degree)` → `Selected degree` (the current ison keeps
  sounding).
- `SelectingIson Nothing` and `NoIson` → `NoIson`.
- `currentDegree`, `appliedAccidentals`, `proposedAccidental`, and
  `proposedMovement` reset as before.

### 3. Ison selection in Listen mode (`Update.processPitchButtonClick`)

In the Listen branch, when `pitchState.ison` is `SelectingIson _`, tapping a
pitch button sets `ison = Selected degree` and clears any accidental on that
degree (mirroring Play mode, since the ison is always a natural pitch).
Otherwise Listen mode still only applies accidentals. The Ison menu, `SetIson`,
and the `i` shortcut are not mode-gated and work in both modes.

### 4. Ison frequency, register, and lazy views (`View.elm`)

- The ison uses `audioSettings.playbackRegister` in both modes. The listen
  register can be `Auto` and flip between Bass and Treble based on detected
  pitch, which would make the ison jump octaves.
- `playbackFrequencyString` computes the frequency for both `<chant-engine>` and
  `<pitch-tracker>`, so both always use identical values.
- Lazy views (`chantEngineNode`, `pitchTracker`) receive frequencies as
  `String`s (`""` for none), not `Maybe Pitch`. `Html.Lazy` compares arguments
  by reference, and a `Just` is rebuilt on every call, which would defeat the
  cache; equal strings compare as equal. `chantEngineNode` takes the gain as a
  `Float` for the same reason (and so unrelated `audioSettings` changes don't
  invalidate it).

### 5. `chant_engine.ts` robustness

- **Resume on demand.** The engine is now mounted before any user gesture, so
  its context can start `suspended`. `playTone` calls `resume()` when needed;
  tones only start in response to user input, which satisfies autoplay policy.
- **Guarded construction.** `new AudioContext()` is wrapped so a failure
  (e.g. `NotSupportedError`) is logged and the engine becomes a silent no-op.

### 6. Microphone handling (`pitch_tracker.ts`)

#### 6a. Mic requested without browser processing

The mic is requested with `echoCancellation`, `noiseSuppression`, and
`autoGainControl` all `false`. This was driven by testing with
`sandbox/ison_click.html` (MacBook speakers):

| Browser | Clicks in the ison when the mic opens/closes |
| --- | --- |
| Chrome | Only with default constraints (`audio: true`), including when the mic is fed into the ison's own context. None with processing off, and none from an extra `AudioContext` alone. |
| Safari | Once, on the first open with default constraints. Otherwise clean. |
| Firefox | With every mic scenario, including processing off. None from an extra `AudioContext` alone. |

Consequences:

- Disabling processing fixes the Chrome click, and gives the pitch detector an
  unprocessed signal (noise suppression and AGC alter the voice; suppression
  can also attenuate held notes).
- Firefox clicks on any mic open/close regardless of constraints, so avoiding it
  requires avoiding mic churn or masking it (see Remaining Work).
- Without echo cancellation the browser no longer attenuates the ison at the
  mic. On speakers this caused real bleed: the ison's pitch was detected during
  silence and disturbed detection of a voice a fifth above it. (No problem on
  headphones.) Hence 6b.
- Lower input level (no AGC) means soft singing can fall under the detector's
  RMS gate (0.01) more often. Not yet observed in practice.

#### 6b. Opt-in ison canceller

- **UI:** `AudioSettings.filterIsonFromMic` (default `False`), shown in Audio
  Settings in Listen mode as a "Speakers" fieldset with an "I'm listening
  through speakers" checkbox and the helper text "Keeps the drone from being
  mistaken for your voice. Leave off when using headphones." The wording is
  deliberately user-facing rather than describing the mechanism. Not persisted
  (neither are the other audio settings).
- **Why opt-in:** headphone users don't need it and shouldn't pay its costs
  (below).
- **Wiring:** Elm emits an `ison` attribute (Hz) on `<pitch-tracker>` only when
  the setting is on and an ison exists (`trackerIsonFrequency`). With no `ison`
  attribute the worklet passes the signal through unchanged, so toggling takes
  effect immediately without restarting the mic.
- **Algorithm:** an `AudioWorklet` between the mic source and the analyser runs
  a two-weight LMS canceller. It generates `sin`/`cos` at the ison frequency
  locally (no playback signal needed, so it works across separate
  `AudioContext`s) and learns the amplitude and phase of the ison at the mic:

  ```
  s = sin(2πft), c = cos(2πft)
  y = w1*s + w2*c                // estimate of the ison as heard at the mic
  e = x - y                      // output → analyser
  w1 += mu*e*s;  w2 += mu*e*c
  ```

- **Implementation notes:**
  - The worklet source is a string in `pitch_tracker.ts`, loaded from a Blob
    URL, so there is no separate worklet file to build, hash
    (`scripts/applyHash.mjs`), or serve. If `AudioWorklet` is unavailable or
    fails to load, the tracker analyzes the mic directly.
  - Tunables in `ISON_CANCELLER_CONFIG`: steady-state notch width (`bandwidthHz`,
    1 Hz); a wider notch (`acquireBandwidthHz`, 10 Hz) for `acquireSeconds` after
    the ison changes; and `harmonics` (1 = fundamental only; raise to 2 if small
    speakers radiate a distorted 2nd harmonic that still bleeds through).
  - `init()` ignores a mic stream that arrives after the element was
    disconnected or re-initialized (the stream is stopped instead of leaking).
- **Offline results** (`sandbox/canceller_check.mjs`, synthetic signals at
  48 kHz): bleed alone is removed to ~0 within ~0.5 s; a voice a fifth above
  the ison (330 vs 220 Hz) is preserved at full amplitude with the ison
  residual ~0; a voice 3 Hz sharp of the ison is preserved (~99%); a retuned
  ison is re-acquired within ~0.1 s; with no ison the signal passes through
  unchanged.
- **Known costs:** a voice within about ±1 Hz of the ison (at unison and nearly
  perfectly in tune) is partially cancelled, and a voice with wide vibrato
  centred on the ison loses roughly 40% of its amplitude (synthetic test). The
  spectrum visualization shows a notch at the ison, since it reads the
  post-cancellation signal. A fixed `BiquadFilterNode` notch was rejected: it
  removes the singer's fundamental at unison unconditionally.
- **Not covered:** room noise, mains hum, other playing audio, or speaker
  harmonics beyond `harmonics`.

## Performance

Measured offline with `sandbox/perf_check.mjs` (node/V8 on a development
machine, so use the ratios, not the absolute numbers):

| Work | Cost |
| --- | --- |
| Worklet, no ison (passthrough, i.e. setting off) | ~0.03 ms per second of audio (negligible) |
| Worklet, filter on, 1 harmonic | ~5 ms per second of audio (~0.5% of a core); ~14 µs per 2667 µs render quantum |
| Worklet, 2 / 3 harmonics | ~11 / ~16 ms per second of audio |
| `autoCorrelate` (pre-existing), 2048-sample window | ~2.3 ms per call; at 60 fps ~140 ms per second (~14% of a core, ~14% of each frame), on the main thread |

The canceller is ~25x cheaper than the detector it feeds, so it is not a
concern even on a 5-10x slower phone CPU. The pre-existing `autoCorrelate`
(O(N²) per animation frame, plus per-frame array allocations) is the main CPU
cost in Listen mode and the likelier problem on weak phones, more so on 120 Hz
displays.

## Remaining Work

**Verification**

- Safari (macOS), Firefox, and a physical iPhone: ison persists across mode
  switches; the speakers setting works; opening directly in Listen mode and
  selecting an ison produces sound (resume-on-demand).
- iOS: with the mic active, iOS uses a play-and-record audio session, so ison
  playback may be quieter or route to the earpiece. Not fixable from Web Audio.

**Decisions**

- **Mic open/close clicks in Firefox (and once in Safari).** Options: keep the
  mic stream open and pause analysis instead of tearing it down (the browser's
  mic-in-use indicator then stays on in Play mode; could release after an idle
  timeout), or briefly duck the ison around the transition. Or accept them.

**Follow-ups**

- **Downsample in `autoCorrelate`.** Decimating the mic signal (e.g. by 4, after
  a simple low-pass/averaging step) before autocorrelating would cut the work
  roughly 16x; voice fundamentals are well under 1 kHz. To explore:
  - Verify accuracy against the current implementation with the offline
    harness (synthetic voice-like signals across the full register range, plus
    recorded singing if available). The parabolic interpolation operates on lag,
    which becomes coarser after decimation, so sub-cent precision matters.
  - Check the lowest (Bass register) frequencies still get at least two periods
    in the analysis window.
  - Re-measure with `sandbox/perf_check.mjs` and, ideally, on a real low-end
    phone.
  - Consider combining with a bounded lag range. Running detection at a lower
    rate is another option, but the smoothing counters are frame-based.
- **Suspend idle `<chant-engine>` context.** Its `AudioContext` stays open in
  Listen mode even when no tone sounds (unmeasured battery cost).
- **Persist the speakers setting** (requires localStorage for settings
  generally).
- **Shared `AudioContext`** module for both elements (one clock, fewer system
  resources). Optional.
- **Separate ison gain** (`ison-gain` attribute) if the drone is too loud next
  to the singer or bleeds too much; defer unless testing shows a need.
- **Changelog entry** in `CHANGELOG.md`.
- **Sandbox scripts:** make `canceller_check.mjs` an asserting `npm run` check
  and share the worklet-extraction code between the `.mjs` scripts.

**Open questions**

- Do other `handleKeydown` shortcuts (movement, etc.) behave sensibly in Listen
  mode now that the engine is live?
- Should the ison register be a dedicated setting rather than `playbackRegister`?
- `ChantEngine.disconnectedCallback` closes the context irreversibly. Fine while
  the element stays mounted; if it is ever conditionally mounted again, the
  engine will need to create its context lazily.

## Acceptance Criteria

- [x] Switching Play ↔ Listen keeps the ison sounding without an audible click
      (Chrome, MacBook speakers).
- [x] With the speakers setting on, the ison is not detected as the singer's
      pitch during silence and doesn't disturb detection a fifth above it
      (Chrome, MacBook speakers).
- [x] No bleed problem on headphones (AirPods), with the setting off.
- [ ] Melos behaves as before in Play mode, and no melos ever sounds in Listen
      mode.
- [ ] In Listen mode the ison can be set via the Ison menu, the `i` shortcut,
      and pitch buttons while selecting; otherwise pitch buttons still apply
      accidentals only.
- [ ] Clearing the ison in Listen mode silences it with the existing cross-fade.
- [ ] Opening the app directly in Listen mode and selecting an ison produces
      sound in Chrome and Safari.
- [ ] Safari, Firefox, and iPhone behave acceptably (see Remaining Work).

## Test Plan

- **Automated:** `tests/Tests.elm` covers `PitchState.resetKeepingIson`. The
  Listen-branch ison selection in `processPitchButtonClick` is private to
  `Update` and not unit-tested. `sandbox/canceller_check.mjs` and
  `sandbox/perf_check.mjs` (`npx tsc && node sandbox/<file>.mjs`) check the
  canceller numerically and benchmark CPU cost; both read the compiled
  `public/dist/src/pitch_tracker.js`.
- **Manual:** Chrome, Firefox, Safari on macOS, and a physical iPhone; headphones
  and laptop speakers; singer at, near, and an octave and fifth from the ison;
  ison changed while the detector is running. Watch the console for
  `NotSupportedError` or autoplay warnings. Use `sandbox/ison_click.html` to
  bisect any new mic open/close clicks.
