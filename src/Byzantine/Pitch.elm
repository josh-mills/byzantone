module Byzantine.Pitch exposing
    ( pitchPosition, pitchPositions
    , Interval, intervalsFrom
    )

{-| Pitch positions and derived intervals. Di is fixed at 84.

In actual practice, this will need to be based on the tetrachord for a given
mode, not a fixed position based merely on the scale. So there will be
additional complexity that we'll need to model somehow. The same with with
attractions and inflections.


# Pitch Positions

@docs pitchPosition, pitchPositions


# Intervals

@docs Interval, intervalsFrom

-}

import Array exposing (Array)
import Byzantine.Degree as Degree exposing (Degree)
import Byzantine.Scale exposing (Scale(..))



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
    Array.fromList [ 0, 12, 24, 30, 42, 54, 60, 72, 84, 96, 102, 114, 126, 132, 144 ]


softChromaticPitchPositions : Array Int
softChromaticPitchPositions =
    Array.fromList [ 0, 8, 22, 30, 42, 50, 64, 72, 84, 92, 106, 114, 126, 134, 148 ]


hardChromaticPitchPositions : Array Int
hardChromaticPitchPositions =
    Array.fromList [ 0, 12, 18, 38, 42, 54, 60, 80, 84, 96, 102, 122, 126, 138, 150 ]



-- INTERVALS


type alias Interval =
    { from : Degree
    , to : Degree
    , moria : Int
    }


getInterval : Scale -> Degree -> Degree -> Interval
getInterval scale from to =
    { from = from
    , to = to
    , moria = pitchPosition scale to - pitchPosition scale from
    }


intervalsFrom : Scale -> Degree -> Degree -> List Interval
intervalsFrom scale lower upper =
    let
        go degrees =
            case degrees of
                a :: b :: rest ->
                    getInterval scale a b :: go (b :: rest)

                _ ->
                    []
    in
    Degree.range lower upper
        |> go
