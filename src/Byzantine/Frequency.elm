module Byzantine.Frequency exposing
    ( Frequency(..), displayString, preciseString
    , PitchStandard(..), pitchStandardToString
    , frequency, toPitchPosition
    )

{-| Frequency-related types and functions.


# Frequency

@docs Frequency, displayString, preciseString


# Pitch Standard

@docs PitchStandard, pitchStandardToString


# Calculations

@docs frequency, toPitchPosition

-}

import Byzantine.Pitch as Pitch exposing (PitchPosition)
import Byzantine.Register as Register exposing (Register)
import Round


{-| Frequency represented in Hz
-}
type Frequency
    = Frequency Float


{-| Rounded to two decimal points with "Hz" label
-}
displayString : Frequency -> String
displayString (Frequency frequency_) =
    Round.round 2 frequency_ ++ " Hz"


{-| Unrounded toFloat (for audio processing)
-}
preciseString : Frequency -> String
preciseString (Frequency frequency_) =
    String.fromFloat frequency_


{-| Pitch standard for frequency. Ni = 256 Hz is the default standard, but the
slightly higher Ke = 440 Hz is available to align with the A440 western
classical standard.
-}
type PitchStandard
    = Ni256
    | Ke440


{-| Convert a PitchStandard to a String representation
-}
pitchStandardToString : PitchStandard -> String
pitchStandardToString pitchStandard =
    case pitchStandard of
        Ni256 ->
            "Ni256"

        Ke440 ->
            "Ke440"


{-| Di is used as a fixed point of reference. Returns the frequency in Hz for Di
based on the given pitch standard.

  - Ni256: Di = 384.0 Hz (based on Ni = 256 Hz)
  - Ke440: Di = 391.995 Hz (based on Ke = 440 Hz)

-}
diFrequency : PitchStandard -> Float
diFrequency pitchStandard =
    case pitchStandard of
        Ni256 ->
            384.0

        Ke440 ->
            391.995


{-| Calculate frequency relative to a fixed pitch for Natural Di, according to
the given pitch standard and register.

Takes a PitchPosition and converts it to a frequency in Hz. The pitch position
is relative to Natural Di at position 84.

-}
frequency : PitchStandard -> Register -> PitchPosition -> Frequency
frequency pitchStandard register pitchPos =
    let
        position =
            toFloat (Pitch.unwrapPitchPosition pitchPos - 84)
    in
    2 ^ (position / 72) * diFrequency pitchStandard * Register.factor register |> Frequency


{-| For a given frequency (in Hz), evaluate the pitch position in moria relative
to a fixed position of Natural Di of 84.

This is the inverse operation of the `frequency` function.

-}
toPitchPosition : PitchStandard -> Register -> Frequency -> Float
toPitchPosition pitchStandard register (Frequency frequency_) =
    72 * logBase 2 (frequency_ / (diFrequency pitchStandard * Register.factor register)) + 84
