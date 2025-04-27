module Byzantine.Pitch exposing
    ( Pitch(..), from, mapDegree, unwrapDegree
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

@docs Pitch, from, mapDegree, unwrapDegree


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



-- PITCH POSITIONS


{-| Calculate the pitch position in moria for the degree. Di is constant at 84.

(We might want an advanced setting to use Ke for mode II instead as equivalent
to Di for other modes; see discussion in Melling.)

-}
pitchPosition : Scale -> Degree -> Int
pitchPosition scale degree =
    pitchPositions scale
        |> Array.get (Degree.indexOf degree)
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


{-| Frequency relative to a fixed pitch for Di, according to the given pitch
standard and register.
-}
frequency : PitchStandard -> Register -> Scale -> Pitch -> Float
frequency pitchStandard register scale pitch =
    let
        ( degree, inflection ) =
            case pitch of
                Natural degree_ ->
                    ( degree_, 0 )

                Inflected accidental degree_ ->
                    ( degree_, Accidental.moriaAdjustment accidental )

        position =
            pitchPosition scale degree - 84 |> (+) inflection |> toFloat

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
    { from : Degree
    , to : Degree
    , moria : Int
    }


getInterval : Scale -> Degree -> Degree -> Interval
getInterval scale from_ to =
    { from = from_
    , to = to
    , moria = pitchPosition scale to - pitchPosition scale from_
    }


intervals : Scale -> List Interval
intervals scale =
    intervalsHelper scale Degree.gamutList


intervalsFrom : Scale -> Degree -> Degree -> List Interval
intervalsFrom scale lower upper =
    intervalsHelper scale (Degree.range lower upper)


intervalsHelper : Scale -> List Degree -> List Interval
intervalsHelper scale degrees =
    case degrees of
        a :: b :: rest ->
            getInterval scale a b :: intervalsHelper scale (b :: rest)

        _ ->
            []
