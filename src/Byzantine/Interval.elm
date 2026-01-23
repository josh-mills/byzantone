module Byzantine.Interval exposing
    ( Interval
    , create, unwrapSize
    , encode, decode
    )

{-| Interval-related types and functions.


# Types

@docs Interval


# Functions

@docs create, unwrapSize


# Encode/Decode

@docs encode, decode

-}

import Byzantine.IntervalSize as IntervalSize exposing (IntervalSize)
import Byzantine.Pitch as Pitch exposing (Pitch)
import Byzantine.Scale as Scale exposing (Scale)


type alias Interval =
    { from : Pitch
    , to : Pitch
    , size : IntervalSize
    }


{-| Create an Interval from two pitches, calculating positions from the given
scale.
-}
create : Scale -> Pitch -> Pitch -> Interval
create scale fromPitch toPitch =
    { from = fromPitch
    , to = toPitch
    , size = IntervalSize.fromPitches scale fromPitch toPitch
    }


{-| Get the size in moria from an Interval.
-}
unwrapSize : Interval -> Int
unwrapSize interval =
    IntervalSize.toInt interval.size


{-| Encode an interval into a string representation.
-}
encode : Scale -> Interval -> String
encode scale interval =
    Pitch.encode scale interval.from
        ++ "~"
        ++ String.fromInt (unwrapSize interval)
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
                (\( fromScale, fromPitch ) expectedMoria ( toScale, toPitch ) ->
                    let
                        interval =
                            create fromScale fromPitch toPitch
                    in
                    if fromScale /= toScale then
                        Err
                            ("Scale mismatch: from pitch uses "
                                ++ Scale.encode fromScale
                                ++ " but to pitch uses "
                                ++ Scale.encode toScale
                            )

                    else if unwrapSize interval /= expectedMoria then
                        Err
                            ("Moria mismatch: expected "
                                ++ String.fromInt expectedMoria
                                ++ " but calculated "
                                ++ String.fromInt (unwrapSize interval)
                            )

                    else
                        Ok interval
                )
                (Pitch.decode fromPitchStr)
                (String.toInt moriaStr
                    |> Result.fromMaybe ("Invalid moria value: " ++ moriaStr)
                )
                (Pitch.decode toPitchStr)
                |> Result.andThen identity

        _ ->
            Err ("Invalid interval format: " ++ intervalString)
