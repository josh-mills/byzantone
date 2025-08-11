module Byzantine.Pitch exposing
    ( Pitch
    , natural, inflected, from, wrapDegree, applyAccidental
    , PitchString, encode, decode
    , unwrapDegree, unwrapAccidental
    , isInflected, isValidInflection, toString
    , pitchPosition, pitchPositions
    , frequency
    , PitchStandard(..), pitchStandardToString
    , Register(..), registerToString
    , Interval, intervals, intervalsFrom, getInterval
    )

{-| Pitch positions and derived intervals. Di is fixed at 84.

In actual practice, this will need to be based on the tetrachord for a given
mode, not a fixed position based merely on the scale. So there will be
additional complexity that we'll need to model somehow. The same with with
attractions and inflections.


# Pitch

@docs Pitch


## Construct

@docs natural, inflected, from, wrapDegree, applyAccidental


## Encode

@docs PitchString, encode, decode


## Unwrap

@docs unwrapDegree, unwrapAccidental


## Misc

@docs isInflected, isValidInflection, toString


## Pitch Positions

@docs pitchPosition, pitchPositions


# Frequency

@docs frequency


## PitchStandard

@docs PitchStandard, pitchStandardToString


## Register

@docs Register, registerToString


# Intervals

@docs Interval, intervals, intervalsFrom, getInterval

-}

import Array exposing (Array)
import Byzantine.Accidental as Accidental exposing (Accidental)
import Byzantine.Degree as Degree exposing (Degree)
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


{-| Wrap a degree as a Natural pitch, unless the current pitch or proposed
movement happens to be inflected and the same degree. In that case, this will
evaluate to a pitch wrapping the inflected degree.
-}
wrapDegree : Maybe Pitch -> Maybe Pitch -> Degree -> Pitch
wrapDegree currentPitch proposedMovementTo degree =
    if Maybe.map unwrapDegree currentPitch == Just degree then
        Maybe.withDefault (Natural degree) currentPitch

    else if Maybe.map unwrapDegree proposedMovementTo == Just degree then
        Maybe.withDefault (Natural degree) proposedMovementTo

    else
        Natural degree


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



-- FREQUENCY


{-| Pitch standard for frequency. Ni = 256 Hz is the default standard, but the
slightly higher Ke = 440 Hz is available to align with the A440 western
classical standard.
-}
type PitchStandard
    = Ni256
    | Ke440


pitchStandardToString : PitchStandard -> String
pitchStandardToString pitchStandard =
    case pitchStandard of
        Ni256 ->
            "Ni256"

        Ke440 ->
            "Ke440"


type Register
    = Treble
    | Bass


registerToString : Register -> String
registerToString register =
    case register of
        Treble ->
            "Treble"

        Bass ->
            "Bass"


{-| Frequency relative to a fixed pitch for Natural Di, according to the given pitch
standard and register.
-}
frequency : PitchStandard -> Register -> Scale -> Pitch -> Float
frequency pitchStandard register scale pitch =
    let
        position =
            pitchPosition scale pitch - 84 |> toFloat

        di =
            case pitchStandard of
                Ni256 ->
                    384.0

                Ke440 ->
                    391.995

        registerFactor =
            case register of
                Treble ->
                    1.0

                Bass ->
                    0.5
    in
    2 ^ (position / 72) * di * registerFactor



-- INTERVALS


type alias Interval =
    { from : Pitch
    , to : Pitch
    , moria : Int
    }


getInterval : Scale -> Pitch -> Pitch -> Interval
getInterval scale from_ to =
    { from = from_
    , to = to
    , moria = pitchPosition scale to - pitchPosition scale from_
    }


{-| Intervals between all natural degrees in the given scale
-}
intervals : Scale -> Maybe Pitch -> Maybe Pitch -> List Interval
intervals scale currentPitch proposedMovementTo =
    Degree.gamutList
        |> List.map (wrapDegree currentPitch proposedMovementTo)
        |> intervalsHelper scale


intervalsFrom : Scale -> Pitch -> Pitch -> List Interval
intervalsFrom scale lower upper =
    Degree.range (unwrapDegree lower) (unwrapDegree upper)
        |> List.map Natural
        |> intervalsHelper scale


intervalsHelper : Scale -> List Pitch -> List Interval
intervalsHelper scale degrees =
    case degrees of
        a :: b :: rest ->
            getInterval scale a b :: intervalsHelper scale (b :: rest)

        _ ->
            []
