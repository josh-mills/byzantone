module Byzantine.Pitch exposing
    ( Pitch
    , natural, from, wrapDegree, applyAccidental
    , unwrapDegree, unwrapAccidental
    , mapDegree, isInflected
    , pitchPosition, pitchPositions
    , PitchStandard(..), Register(..), frequency
    , Interval, intervals, intervalsFrom
    , degreeCanSupportAccidental, inflected
    )

{-| Pitch positions and derived intervals. Di is fixed at 84.

In actual practice, this will need to be based on the tetrachord for a given
mode, not a fixed position based merely on the scale. So there will be
additional complexity that we'll need to model somehow. The same with with
attractions and inflections.


# Pitch

@docs Pitch


## Construct

@docs natural, from, wrapDegree, applyAccidental


## Unwrap

@docs unwrapDegree, unwrapAccidental


## Misc

@docs mapDegree, isInflected


## Pitch Positions

@docs pitchPosition, pitchPositions


# Frequency

@docs PitchStandard, Register, frequency


# Intervals

@docs Interval, intervals, intervalsFrom

-}

import Array exposing (Array)
import Byzantine.Accidental as Accidental exposing (Accidental)
import Byzantine.Degree as Degree exposing (Degree)
import Byzantine.Scale exposing (Scale(..))
import Maybe.Extra
import Result exposing (Result)


type Pitch
    = Natural Degree
    | Inflected Accidental Degree



-- CONSTRUCT


from : Maybe Accidental -> Degree -> Pitch
from maybeAccidental degree =
    case maybeAccidental of
        Just accidental ->
            Inflected accidental degree

        Nothing ->
            Natural degree


{-| Wrap a degree as a `Natural` pitch.
-}
natural : Degree -> Pitch
natural degree =
    Natural degree


{-| Wrap a degree as an `Inflected` pitch.

TODO: add validation.

-}
inflected : Scale -> Accidental -> Degree -> Result String Pitch
inflected scale accidental degree =
    if degreeCanSupportAccidental scale accidental degree then
        Ok (Inflected accidental degree)

    else
        Err "nope"


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


applyAccidental : Maybe Accidental -> Pitch -> Pitch
applyAccidental maybeAccidental pitch =
    case maybeAccidental of
        Nothing ->
            Natural (unwrapDegree pitch)

        Just accidental ->
            case pitch of
                Natural degree ->
                    Inflected accidental degree

                Inflected _ degree ->
                    Inflected accidental degree



-- VALIDATION


{-| Given the scale, does it make sense for the proposed accidental be applied
to the given degree? An inflected pitch cannot be at or beyond the pitch
position of the next degree in the direction of inflection.

TODO: can we move this to the Pitch module and bake into the `applyAccidental`
logic? Maybe even make `Pitch` opaque?

-}
degreeCanSupportAccidental : Scale -> Accidental -> Degree -> Bool
degreeCanSupportAccidental scale accidental degree =
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


{-| TODO: I'm not sure this actually makes a lot of domain sense.
Is this something we can get rid of?
-}
mapDegree : (Degree -> Degree) -> Pitch -> Pitch
mapDegree f pitch =
    case pitch of
        Natural degree ->
            Natural (f degree)

        Inflected accidental degree ->
            Inflected accidental (f degree)


isInflected : Pitch -> Bool
isInflected pitch =
    case pitch of
        Natural _ ->
            False

        Inflected _ _ ->
            True



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
                    ( degree_, (+) <| Accidental.moriaAdjustment accidental )
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


type Register
    = Treble
    | Bass


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

the currentPitch needs to be taken into account

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
