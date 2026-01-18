module Byzantine.IntervalSize exposing
    ( IntervalSize
    , fromPitches
    , unwrap, toInt
    )

{-| Interval Size in moria - represents the difference between two pitch positions.

This module encapsulates all calculations related to the size of intervals,
providing a clean abstraction over the underlying integer moria values.


# Type

@docs IntervalSize


# Construction

@docs fromPitches


# Conversion

@docs unwrap, toInt

-}

import Byzantine.Pitch as Pitch exposing (Pitch)
import Byzantine.PitchPosition as PitchPosition
import Byzantine.Scale exposing (Scale)


{-| Interval Size in moria. Represents the difference between two
PitchPositions.
-}
type IntervalSize
    = IntervalSize Int


{-| Create an IntervalSize from two pitches within a scale by calculating the difference
between their pitch positions.
-}
fromPitches : Scale -> Pitch -> Pitch -> IntervalSize
fromPitches scale fromPitch toPitch =
    let
        fromPosition =
            Pitch.position scale fromPitch

        toPosition =
            Pitch.position scale toPitch
    in
    IntervalSize (PitchPosition.unwrap toPosition - PitchPosition.unwrap fromPosition)


{-| Unwrap an IntervalSize to get the underlying integer moria value.
-}
unwrap : IntervalSize -> Int
unwrap (IntervalSize value) =
    value


{-| Convert an IntervalSize to an integer (alias for unwrap for clarity).
-}
toInt : IntervalSize -> Int
toInt intervalSize =
    unwrap intervalSize
