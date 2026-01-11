module Byzantine.Utils exposing (isValidInflection)

{-| Utility functions for Byzantine music theory calculations.


# Validation

@docs isValidInflection

-}

import Byzantine.Accidental as Accidental exposing (Accidental)
import Byzantine.Degree as Degree exposing (Degree)
import Byzantine.PitchPosition as PitchPosition
import Byzantine.Scale exposing (Scale)
import Maybe.Extra


{-| Given the scale, does it make sense for the proposed accidental be applied
to the given degree? An inflected pitch cannot be at or beyond the pitch
position of the next degree in the direction of inflection.
-}
isValidInflection : Scale -> Accidental -> Degree -> Bool
isValidInflection scale accidental degree =
    let
        proposedPitchPosition =
            PitchPosition.pitchPosition scale degree (Just accidental)
                |> PitchPosition.unwrap

        naturalPosition degree_ =
            PitchPosition.pitchPosition scale degree_ Nothing
                |> PitchPosition.unwrap
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
