module Byzantine.Scale exposing (Degree(..), Scale(..), all, name)

{-| Scales
-}


{-| Boyer calls the diatonic the "soft-diatonic scale", and the enharmonic,
"hard-diatonic".
-}
type Scale
    = Diatonic
    | Enharmonic
    | SoftChromatic
    | HardChromatic


all : List Scale
all =
    [ Diatonic, Enharmonic, SoftChromatic, HardChromatic ]


name : Scale -> String
name scale =
    case scale of
        Diatonic ->
            "Diatonic"

        Enharmonic ->
            "Enharmonic"

        SoftChromatic ->
            "Soft Chromatic"

        HardChromatic ->
            "Hard Chromatic"


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
