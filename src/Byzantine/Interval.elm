module Byzantine.Interval exposing
    ( Interval
    , create, size
    , encode, decode
    )

{-| Interval-related types and functions.


# Types

@docs Interval


# Functions

@docs create, size


# Encode/Decode

@docs encode, decode

-}

import Byzantine.Pitch as Pitch exposing (Pitch)
import Byzantine.PitchPosition as PitchPosition exposing (PitchPosition)
import Byzantine.Scale as Scale exposing (Scale)
import Result exposing (Result)


type alias Interval =
    { from : Pitch
    , to : Pitch
    , moria : Size
    }


{-| Interval Size in moria. Represents the difference between two
PitchPositions.
-}
type Size
    = Size Int


{-| Create an Interval from two pitches, calculating positions from the given
scale.
-}
create : Scale -> Pitch -> Pitch -> Interval
create scale fromPitch toPitch =
    { from = fromPitch
    , to = toPitch
    , moria = calculateSize (Pitch.position scale fromPitch) (Pitch.position scale toPitch)
    }


calculateSize : PitchPosition -> PitchPosition -> Size
calculateSize fromPitch toPitch =
    Size (PitchPosition.unwrap toPitch - PitchPosition.unwrap fromPitch)


{-| Get the size in moria from an Interval.
-}
size : Interval -> Int
size interval =
    case interval.moria of
        Size value ->
            value


{-| Encode an interval into a string representation.
-}
encode : Scale -> Interval -> String
encode scale interval =
    Pitch.encode scale interval.from
        ++ "~"
        ++ String.fromInt (size interval)
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
                            , moria = Size moria
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
