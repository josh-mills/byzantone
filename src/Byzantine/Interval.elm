module Byzantine.Interval exposing
    ( Interval, IntervalSize
    , intervalSize, unwrapIntervalSize
    , encode, decode
    )

{-| Interval-related types and functions.


# Types

@docs Interval, IntervalSize


# IntervalSize

@docs intervalSize, unwrapIntervalSize


# Encode/Decode

@docs encode, decode

-}

import Byzantine.Pitch as Pitch exposing (Pitch, PitchPosition)
import Byzantine.Scale as Scale exposing (Scale)
import Result exposing (Result)



-- INTERVAL SIZE


{-| Interval Size in moria. Represents the difference between two PitchPositions.
-}
type IntervalSize
    = IntervalSize Int


{-| Create an IntervalSize from the difference between two PitchPositions.
-}
intervalSize : PitchPosition -> PitchPosition -> IntervalSize
intervalSize toPos fromPos =
    IntervalSize (Pitch.unwrapPitchPosition toPos - Pitch.unwrapPitchPosition fromPos)


{-| Unwrap an IntervalSize to get the Int value.
-}
unwrapIntervalSize : IntervalSize -> Int
unwrapIntervalSize (IntervalSize value) =
    value



-- INTERVALS


{-| TODO: this should be reconsidered.
-}
type alias Interval =
    { from : Pitch
    , to : Pitch
    , moria : IntervalSize
    }


{-| Encode an interval into a string representation.
-}
encode : Scale -> Interval -> String
encode scale interval =
    Pitch.encode scale interval.from
        ++ "~"
        ++ String.fromInt (unwrapIntervalSize interval.moria)
        ++ "~"
        ++ Pitch.encode scale interval.to


{-| Decode a string representation of an interval back into an Interval record.
The string must be in the format produced by encode:
`<encoded_from_pitch>~<moria>~<encoded_to_pitch>`

Both pitches must use the same scale, otherwise an error is returned.

-}
decode : String -> Result String Interval
decode intervalString =
    case String.split "~" intervalString of
        [ fromPitchStr, moriaStr, toPitchStr ] ->
            Result.map3
                (\( fromScale, fromPitch ) moria ( toScale, toPitch ) ->
                    if fromScale == toScale then
                        Ok
                            { from = fromPitch
                            , to = toPitch
                            , moria = IntervalSize moria
                            }

                    else
                        Err
                            ("Scale mismatch: from pitch uses "
                                ++ Scale.encode fromScale
                                ++ " but to pitch uses "
                                ++ Scale.encode toScale
                            )
                )
                (Pitch.decode fromPitchStr)
                (String.toInt moriaStr
                    |> Result.fromMaybe ("Invalid moria value: " ++ moriaStr)
                )
                (Pitch.decode toPitchStr)
                |> Result.andThen identity

        _ ->
            Err ("Invalid interval format: " ++ intervalString)
