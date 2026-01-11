module Byzantine.PitchPosition exposing
    ( PitchPosition
    , pitchPosition, pitchPositions, unwrap, toFloat
    )

{-| Pitch positions in moria. Di is fixed at 84.

In actual practice, this will need to be based on the tetrachord for a given
mode, not a fixed position based merely on the scale. So there will be
additional complexity that we'll need to model somehow. The same with with
attractions and inflections.


# Pitch Position

@docs PitchPosition


## Functions

@docs pitchPosition, pitchPositions, unwrap, toFloat

-}

import Array exposing (Array)
import Byzantine.Accidental as Accidental exposing (Accidental)
import Byzantine.Degree as Degree exposing (Degree)
import Byzantine.Scale exposing (Scale(..))


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


{-| Calculate the pitch position in moria for the given degree and optional accidental.
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
