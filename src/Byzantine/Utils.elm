module Byzantine.Utils exposing (degreeCanSupportAccidental)

import Byzantine.Accidental as Accidental exposing (Accidental)
import Byzantine.Degree as Degree exposing (Degree)
import Byzantine.Pitch as Pitch
import Byzantine.Scale exposing (Scale)
import Maybe.Extra


{-| Given the scale, does it make sense for the proposed accidental be applied
to the given degree? An inflected pitch cannot be at or beyond the pitch
position of the next degree in the direction of inflection.
-}
degreeCanSupportAccidental : Scale -> Accidental -> Degree -> Bool
degreeCanSupportAccidental scale accidental degree =
    let
        proposedPitchPosition =
            Pitch.Inflected accidental degree
                |> Pitch.pitchPosition scale

        naturalPosition degree_ =
            Pitch.pitchPosition scale (Pitch.Natural degree_)
    in
    case accidentalInflectionDirection accidental of
        Up ->
            Degree.step degree 1
                |> Maybe.Extra.unwrap False
                    (\nextDegree ->
                        naturalPosition nextDegree > proposedPitchPosition
                    )

        Down ->
            Degree.step degree -1
                |> Maybe.Extra.unwrap False
                    (\nextDegree ->
                        naturalPosition nextDegree < proposedPitchPosition
                    )


type AccidentalInflectionDirection
    = Up
    | Down


accidentalInflectionDirection : Accidental -> AccidentalInflectionDirection
accidentalInflectionDirection accidental =
    if Accidental.moriaAdjustment accidental > 0 then
        Up

    else
        Down
