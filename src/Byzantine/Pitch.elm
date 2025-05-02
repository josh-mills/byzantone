module Byzantine.Pitch exposing
    ( Pitch(..), from, mapDegree, wrapDegree, unwrapDegree, unwrapAccidental
    , pitchPosition, pitchPositions
    , PitchStandard(..), Register(..), frequency
    , Interval, intervals, intervalsFrom
    )

{-| Pitch positions and derived intervals. Di is fixed at 84.

In actual practice, this will need to be based on the tetrachord for a given
mode, not a fixed position based merely on the scale. So there will be
additional complexity that we'll need to model somehow. The same with with
attractions and inflections.


# Pitch

@docs Pitch, from, mapDegree, wrapDegree, unwrapDegree, unwrapAccidental


# Pitch Positions

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


type Pitch
    = Natural Degree
    | Inflected Accidental Degree


from : Maybe Accidental -> Degree -> Pitch
from maybeAccidental degree =
    case maybeAccidental of
        Just accidental ->
            Inflected accidental degree

        Nothing ->
            Natural degree


{-| Wrap a degree as a Natural pitch, unless the current pitch happens to be
inflected and the same degree. In that case, this will evaluate to the inflected
current pitch.
-}
wrapDegree : Maybe Pitch -> Degree -> Pitch
wrapDegree currentPitch degree =
    case currentPitch of
        Just pitch ->
            if degree == unwrapDegree pitch then
                pitch

            else
                Natural degree

        _ ->
            Natural degree


unwrapDegree : Pitch -> Degree
unwrapDegree pitch =
    case pitch of
        Natural degree ->
            degree

        Inflected _ degree ->
            degree


mapDegree : (Degree -> Degree) -> Pitch -> Pitch
mapDegree f pitch =
    case pitch of
        Natural degree ->
            Natural (f degree)

        Inflected accidental degree ->
            Inflected accidental (f degree)


unwrapAccidental : Pitch -> Maybe Accidental
unwrapAccidental pitch =
    case pitch of
        Natural _ ->
            Nothing

        Inflected accidental _ ->
            Just accidental



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
intervals : Scale -> Maybe Pitch -> List Interval
intervals scale currentPitch =
    Degree.gamutList
        |> List.map (wrapDegree currentPitch)
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
