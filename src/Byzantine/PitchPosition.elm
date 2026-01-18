module Byzantine.PitchPosition exposing
    ( PitchPosition
    , pitchPosition, pitchPositions
    , inflect, compare, isValidInflection, unwrap, toFloat
    )

{-| Pitch positions in moria. Di is fixed at 84.

In actual practice, this will need to be based on the tetrachord for a given
mode, not a fixed position based merely on the scale. So there will be
additional complexity that we'll need to model somehow. The same with with
attractions and inflections.


# Pitch Position

@docs PitchPosition
@docs pitchPosition, pitchPositions


## Functions

@docs inflect, compare, isValidInflection, unwrap, toFloat

-}

import Array exposing (Array)
import Byzantine.Accidental as Accidental exposing (Accidental)
import Byzantine.Degree as Degree exposing (Degree)
import Byzantine.Scale exposing (Scale(..))
import Maybe.Extra


{-| Pitch Position in moria. Di is constant at 84.
-}
type PitchPosition
    = PitchPosition Int


{-| Unwrap a PitchPosition to get the Int value.
-}
unwrap : PitchPosition -> Int
unwrap (PitchPosition value) =
    value


{-| Convert a PitchPosition to a Float value.
-}
toFloat : PitchPosition -> Float
toFloat (PitchPosition value) =
    Basics.toFloat value


{-| Calculate the pitch position in moria for the given pitch.
Di is constant at 84.

(We might want an advanced setting to use Ke for mode II instead as equivalent
to Di for other modes; see discussion in Melling.)

-}
pitchPosition : Scale -> Degree -> Maybe Accidental -> PitchPosition
pitchPosition scale degree maybeAccidental =
    let
        basePitchPosition =
            pitchPositions scale
                |> Array.get (Degree.indexOf degree)
                -- tests ensure that the -1 sentinel value never occurs.
                |> Maybe.withDefault (PitchPosition -1)

        moriaAdjustment =
            maybeAccidental
                |> Maybe.map Accidental.moriaAdjustment
                |> Maybe.withDefault 0
    in
    basePitchPosition
        |> unwrap
        |> (+) moriaAdjustment
        |> PitchPosition


{-| Apply an accidental to a pitch position, returning a new pitch position
adjusted by the accidental's moria adjustment.
-}
inflect : PitchPosition -> Accidental -> PitchPosition
inflect position accidental =
    PitchPosition (unwrap position + Accidental.moriaAdjustment accidental)


{-| Compare two pitch positions and return an Order.
-}
compare : PitchPosition -> PitchPosition -> Order
compare position1 position2 =
    Basics.compare (unwrap position1) (unwrap position2)


{-| Get the pitch positions for a given scale.
-}
pitchPositions : Scale -> Array PitchPosition
pitchPositions scale =
    case scale of
        Diatonic ->
            diatonicPitchPositions

        Enharmonic ->
            enharmonicPitchPositions

        SoftChromatic ->
            softChromaticPitchPositions

        HardChromatic ->
            hardChromaticPitchPositions


diatonicPitchPositions : Array PitchPosition
diatonicPitchPositions =
    [ 0, 12, 24, 34, 42, 54, 64, 72, 84, 96, 106, 114, 126, 136, 144 ]
        |> Array.fromList
        |> Array.map PitchPosition


enharmonicPitchPositions : Array PitchPosition
enharmonicPitchPositions =
    [ 0, 12, 24, 30, 42, 54, 66, 72, 84, 96, 102, 114, 126, 138, 144 ]
        |> Array.fromList
        |> Array.map PitchPosition


{-| Michael Tsiappoutas gives slightly different positions for this: he shows 7
for the smaller steps rather than 8, and 9 rather than 10 for others. It may be
worth exploring this and providing this as an option, if this is a standard
tradition to reflect in the app.
-}
softChromaticPitchPositions : Array PitchPosition
softChromaticPitchPositions =
    [ 0, 8, 22, 30, 42, 50, 64, 72, 84, 92, 106, 114, 126, 134, 148 ]
        |> Array.fromList
        |> Array.map PitchPosition


hardChromaticPitchPositions : Array PitchPosition
hardChromaticPitchPositions =
    [ 0, 12, 18, 38, 42, 54, 60, 80, 84, 96, 102, 122, 126, 136, 144 ]
        |> Array.fromList
        |> Array.map PitchPosition



-- VALIDATION


{-| Given the scale, does it make sense for the proposed accidental be applied
to the given degree? An inflected pitch cannot be at or beyond the pitch
position of the next degree in the direction of inflection.
-}
isValidInflection : Scale -> Accidental -> Degree -> Bool
isValidInflection scale accidental degree =
    let
        proposedPitchPosition =
            pitchPosition scale degree (Just accidental)

        naturalPosition degree_ =
            pitchPosition scale degree_ Nothing
    in
    case accidentalInflectionDirection accidental of
        Up ->
            Degree.step degree 1
                |> Maybe.Extra.unwrap False
                    (\nextDegree ->
                        compare (naturalPosition nextDegree) proposedPitchPosition == GT
                    )

        Down ->
            Degree.step degree -1
                |> Maybe.Extra.unwrap False
                    (\nextDegree ->
                        compare (naturalPosition nextDegree) proposedPitchPosition == LT
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
