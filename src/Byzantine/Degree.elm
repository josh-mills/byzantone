module Byzantine.Degree exposing (Degree(..), baseOctave, baseOctaveIntervals, gamut, indexOf, intervalsFrom, pitchPosition, step, toString)

import Array exposing (Array)
import Byzantine.Scale exposing (Scale(..))


{-| The seven basic scale degrees or steps (παραλλαγή) of the Byzantine system:
Πα, Βου, Γα, Δι, Κε, Ζω, and Νη.

The base range extends from Ζω through Κε (`Zo`, `Ni`, `Pa`, `Bou`, `Ga`, `Di`,
and `Ke`). The lower octave is indicated by capitals (`GA`, `DI`, and `KE`), and
the upper octave with an underscore (`Zo_`, `Ni_`, `Pa_`, `Bou_`, and `Ga_`).

I think that in practice, not all scale degrees will be used by every scale.

-}
type Degree
    = GA
    | DI
    | KE
    | Zo
    | Ni
    | Pa
    | Bou
    | Ga
    | Di
    | Ke
    | Zo_
    | Ni_
    | Pa_
    | Bou_
    | Ga_


toString : Degree -> String
toString degree =
    case degree of
        GA ->
            "GA"

        DI ->
            "DI"

        KE ->
            "KE"

        Zo ->
            "Zo"

        Ni ->
            "Ni"

        Pa ->
            "Pa"

        Bou ->
            "Bou"

        Ga ->
            "Ga"

        Di ->
            "Di"

        Ke ->
            "Ke"

        Zo_ ->
            "Zo_"

        Ni_ ->
            "Ni_"

        Pa_ ->
            "Pa_"

        Bou_ ->
            "Bou_"

        Ga_ ->
            "Ga_"


gamut : Array Degree
gamut =
    Array.fromList [ GA, DI, KE, Zo, Ni, Pa, Bou, Ga, Di, Ke, Zo_, Ni_, Pa_, Bou_, Ga_ ]


indexOf : Degree -> Int
indexOf degree =
    case degree of
        GA ->
            0

        DI ->
            1

        KE ->
            2

        Zo ->
            3

        Ni ->
            4

        Pa ->
            5

        Bou ->
            6

        Ga ->
            7

        Di ->
            8

        Ke ->
            9

        Zo_ ->
            10

        Ni_ ->
            11

        Pa_ ->
            12

        Bou_ ->
            13

        Ga_ ->
            14


step : Degree -> Int -> Maybe Degree
step degree interval =
    Array.get (indexOf degree + interval) gamut


baseOctave : Scale -> Array Degree
baseOctave scale =
    let
        octaveFrom degree =
            let
                index =
                    indexOf degree
            in
            Array.slice index (index + 8) gamut
    in
    case scale of
        Diatonic ->
            octaveFrom Pa

        Enharmonic ->
            octaveFrom Ni

        SoftChromatic ->
            octaveFrom Ni

        HardChromatic ->
            octaveFrom Pa


baseOctaveIntervals : Scale -> List Int
baseOctaveIntervals scale =
    let
        startingPitch =
            case scale of
                Diatonic ->
                    Pa

                Enharmonic ->
                    Ni

                SoftChromatic ->
                    Ni

                HardChromatic ->
                    Pa
    in
    pitchPositions scale
        |> Array.slice (indexOf startingPitch) (indexOf startingPitch + 8)
        |> Array.toList
        |> toIntervals


intervalsFrom : Scale -> Degree -> Degree -> List Int
intervalsFrom scale lower upper =
    pitchPositions scale
        |> Array.slice (indexOf lower) (indexOf upper)
        |> Array.toList
        |> toIntervals


toIntervals : List Int -> List Int
toIntervals pitchPositions_ =
    case pitchPositions_ of
        lower :: upper :: rest ->
            upper - lower :: toIntervals (upper :: rest)

        _ ->
            []


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


{-| Calculate the pitch position in moria for the degree. Di is constant at 84.

In actual practice, this will need to be based on the tetrachord for a given
mode, not a fixed position based merely on the scale. So there will be
additional complexity that we'll need to model somehow.

(We might want an advanced setting to use Ke for mode II instead as equivalent
to Di for other modes; see discussion in Melling.)

-}
pitchPosition : Scale -> Degree -> Int
pitchPosition scale degree =
    pitchPositions scale
        |> Array.get (indexOf degree)
        -- tests ensure that the -1 sentinel value never occurs.
        |> Maybe.withDefault -1


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
