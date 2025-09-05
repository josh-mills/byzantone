module Byzantine.Pitch exposing
    ( Pitch
    , natural, inflected, from, applyAccidental
    , PitchString, encode, decode
    , unwrapDegree, unwrapAccidental
    , isInflected, isValidInflection, toString
    , pitchPosition, pitchPositions
    , getPitchFrequency
    , Interval
    )

{-| Pitch positions and derived intervals. Di is fixed at 84.

In actual practice, this will need to be based on the tetrachord for a given
mode, not a fixed position based merely on the scale. So there will be
additional complexity that we'll need to model somehow. The same with with
attractions and inflections.


# Pitch

@docs Pitch


## Construct

@docs natural, inflected, from, applyAccidental


## Encode

@docs PitchString, encode, decode


## Unwrap

@docs unwrapDegree, unwrapAccidental


## Misc

@docs isInflected, isValidInflection, toString


## Pitch Positions

@docs pitchPosition, pitchPositions


# Frequency

@docs getPitchFrequency


# Intervals

@docs Interval

-}

import Array exposing (Array)
import Byzantine.Accidental as Accidental exposing (Accidental)
import Byzantine.Degree as Degree exposing (Degree)
import Byzantine.Frequency as Frequency exposing (Frequency, PitchStandard)
import Byzantine.Register as Register exposing (Register)
import Byzantine.Scale as Scale exposing (Scale(..))
import Maybe.Extra
import Result exposing (Result)
import Tuple.Trio as Trio


type Pitch
    = Natural Degree
    | Inflected Accidental Degree



-- ENCODE


{-| String-encoded representation of a pitch within a scale.
-}
type alias PitchString =
    String


{-| Encode both a scale and pitch into a combined string representation. This
allows decoding without a separate scale argument.
-}
encode : Scale -> Pitch -> PitchString
encode scale pitch =
    (case pitch of
        Natural degree ->
            [ "n", Degree.toString degree ]

        Inflected accidental degree ->
            [ "i", Degree.toString degree, Accidental.toString accidental ]
    )
        |> (::) (Scale.encode scale)
        |> String.join "|"


{-| Decode a string representation that contains both scale and pitch
information. The string must be in the format:

  - "<scale>|n|<degree>" for natural pitches

  - "<scale>|i|<degree>|<accidental>" for inflected pitches

where <scale> is one of the codes defined in Scale.encode.

-}
decode : PitchString -> Result String ( Scale, Pitch )
decode pitchString =
    case String.split "|" pitchString of
        [ scaleCode, "n", degreeStr ] ->
            Result.map2 Tuple.pair
                (Scale.decode scaleCode)
                (Degree.fromString degreeStr
                    |> Result.map Natural
                )

        [ scaleCode, "i", degreeStr, accidentalStr ] ->
            Result.map3 Trio.intrine
                (Scale.decode scaleCode)
                (Degree.fromString degreeStr)
                (Accidental.fromString accidentalStr)
                |> Result.andThen
                    (\( scale, degree, accidental ) ->
                        inflected scale accidental degree
                            |> Result.map (Tuple.pair scale)
                    )

        _ ->
            Err ("Invalid format for scale and pitch: " ++ pitchString)



-- CONSTRUCT


{-| Construct a `Pitch`, which _may_ be inflected. If the degree cannot support
the accidental within the scale, this will default to a natural pitch.
-}
from : Scale -> Maybe Accidental -> Degree -> Pitch
from scale maybeAccidental degree =
    case maybeAccidental of
        Just accidental ->
            if isValidInflection scale accidental degree then
                Inflected accidental degree

            else
                Natural degree

        Nothing ->
            Natural degree


{-| Wrap a degree as a `Natural` pitch.
-}
natural : Degree -> Pitch
natural degree =
    Natural degree


{-| Wrap a degree as an `Inflected` pitch.
-}
inflected : Scale -> Accidental -> Degree -> Result String Pitch
inflected scale accidental degree =
    if isValidInflection scale accidental degree then
        Ok (Inflected accidental degree)

    else
        Err
            (Scale.name scale
                ++ " "
                ++ Degree.toString degree
                ++ " cannot support a "
                ++ Accidental.toString accidental
                ++ " accidental."
            )


{-| Apply the accidental to the given pitch. This will evaluate to Natural
if the accidental is not valid for that degree within the given scale, or if
it is `Nothing`.
-}
applyAccidental : Scale -> Pitch -> Maybe Accidental -> Pitch
applyAccidental scale pitch maybeAccidental =
    let
        degree =
            unwrapDegree pitch
    in
    case maybeAccidental of
        Nothing ->
            Natural degree

        Just accidental ->
            if isValidInflection scale accidental degree then
                Inflected accidental degree

            else
                Natural degree



-- VALIDATION


{-| Given the scale, does it make sense for the proposed accidental be applied
to the given degree? An inflected pitch cannot be at or beyond the pitch
position of the next degree in the direction of inflection.
-}
isValidInflection : Scale -> Accidental -> Degree -> Bool
isValidInflection scale accidental degree =
    let
        proposedPitchPosition =
            Inflected accidental degree
                |> pitchPosition scale

        naturalPosition degree_ =
            Natural degree_
                |> pitchPosition scale
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



-- UNWRAP


unwrapDegree : Pitch -> Degree
unwrapDegree pitch =
    case pitch of
        Natural degree ->
            degree

        Inflected _ degree ->
            degree


unwrapAccidental : Pitch -> Maybe Accidental
unwrapAccidental pitch =
    case pitch of
        Natural _ ->
            Nothing

        Inflected accidental _ ->
            Just accidental



-- MISC


isInflected : Pitch -> Bool
isInflected pitch =
    case pitch of
        Natural _ ->
            False

        Inflected _ _ ->
            True


toString : Pitch -> String
toString pitch =
    case pitch of
        Natural degree ->
            Degree.toString degree

        Inflected accidental degree ->
            Degree.toString degree ++ " " ++ Accidental.toString accidental



-- PITCH POSITIONS


{-| Calculate the pitch position in moria for the degree. Di is constant at 84.

(We might want an advanced setting to use Ke for mode II instead as equivalent
to Di for other modes; see discussion in Melling.)

-}
pitchPosition : Scale -> Pitch -> Int
pitchPosition scale pitch =
    let
        ( degree, moriaAdjustment ) =
            case pitch of
                Natural degree_ ->
                    ( degree_, identity )

                Inflected accidental degree_ ->
                    ( degree_, (+) (Accidental.moriaAdjustment accidental) )
    in
    pitchPositions scale
        |> Array.get (Degree.indexOf degree)
        |> Maybe.map moriaAdjustment
        -- tests ensure that the -1 sentinel value never occurs.
        |> Maybe.withDefault -1


pitchPositions : Scale -> Array Int
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


diatonicPitchPositions : Array Int
diatonicPitchPositions =
    Array.fromList [ 0, 12, 24, 34, 42, 54, 64, 72, 84, 96, 106, 114, 126, 136, 144 ]


enharmonicPitchPositions : Array Int
enharmonicPitchPositions =
    Array.fromList [ 0, 12, 24, 30, 42, 54, 66, 72, 84, 96, 102, 114, 126, 138, 144 ]


{-| Michael Tsiappoutas gives slightly different positions for this: he shows 7
for the smaller steps rather than 8, and 9 rather than 10 for others. It may be
worth exploring this and providing this as an option, if this is a standard
tradition to reflect in the app.
-}
softChromaticPitchPositions : Array Int
softChromaticPitchPositions =
    Array.fromList [ 0, 8, 22, 30, 42, 50, 64, 72, 84, 92, 106, 114, 126, 134, 148 ]


hardChromaticPitchPositions : Array Int
hardChromaticPitchPositions =
    Array.fromList [ 0, 12, 18, 38, 42, 54, 60, 80, 84, 96, 102, 122, 126, 136, 144 ]


{-| Calculate frequency for a pitch using the given pitch standard and register.
-}
getPitchFrequency : PitchStandard -> Register -> Scale -> Pitch -> Frequency
getPitchFrequency pitchStandard register scale pitch =
    pitchPosition scale pitch
        |> Frequency.frequency pitchStandard register



-- INTERVALS


{-| TODO: this should be reconsidered.
-}
type alias Interval =
    { from : Pitch
    , to : Pitch
    , moria : Int
    }
