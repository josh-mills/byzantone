module Byzantine.IntervalSize exposing (IntervalSize, fromPitches, toInt)

{-| Interval size between two pitches.

@docs IntervalSize, fromPitches, toInt

-}

import Byzantine.Pitch as Pitch exposing (Pitch)
import Byzantine.PitchPosition as PitchPosition
import Byzantine.Scale exposing (Scale)


{-| Interval Size in moria. Represents the difference between two
PitchPositions.
-}
type IntervalSize
    = IntervalSize Int


{-| Create an IntervalSize from two pitches within a scale by calculating the
difference between their pitch positions.
-}
fromPitches : Scale -> Pitch -> Pitch -> IntervalSize
fromPitches scale fromPitch toPitch =
    let
        fromPosition =
            PitchPosition.unwrap (Pitch.position scale fromPitch)

        toPosition =
            PitchPosition.unwrap (Pitch.position scale toPitch)
    in
    IntervalSize (toPosition - fromPosition)


{-| Unwrap underlying Int value
-}
toInt : IntervalSize -> Int
toInt (IntervalSize value) =
    value
