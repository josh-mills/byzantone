module Byzantine.Degree exposing (Degree(..), baseOctave, gamut, indexOf, range, step, toString)

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


{-| List of steps from `from` to `to`, inclusive.
-}
range : Degree -> Degree -> List Degree
range from to =
    Array.slice (indexOf from) (indexOf to + 1) gamut
        |> Array.toList


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
