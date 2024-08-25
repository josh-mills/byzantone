module Byzantine.Martyria exposing
    ( Martyria
    , ModalSignature(..)
    , for
    , unwrap
    )

{-| The μαρτυρἰαι ("testimonies"), or signatures, of Byzantine music,
representing a note within the context of a given modal context.
-}

import Byzantine.Degree exposing (Degree(..))
import Byzantine.Scale exposing (Scale(..))


{-| The μαρτυρἰαι ("testimonies"), or signatures, of Byzantine music,
representing a note within the context of a given modal context.

Because a martyria can be derived from the scale and the scale degree, the type
is opaque so that only valid combinations can be created.

-}
type Martyria
    = Martyria Degree ModalSignature


unwrap : Martyria -> ( Degree, ModalSignature )
unwrap (Martyria degree signature) =
    ( degree, signature )


{-| A modal signature (μαπτυρικὀν σημείον, "testimonial sign") is a shorthand
representation of the mode based on a given note. This indicates what scale
scale system a degree is operating within.
-}
type ModalSignature
    = Baris
    | Delta
    | Alpha
    | Legetos
    | NaNa
    | DeltaDotted
    | AlphaDotted
    | BetaHard
    | NenanoHard
    | BetaSoft
    | NenanoSoft


{-| Construct the maryria for a given scale and degree.
-}
for : Scale -> Degree -> Martyria
for scale degree =
    case scale of
        Diatonic ->
            Martyria degree (diatonicSignature degree)

        HardChromatic ->
            Martyria degree (hardChromaticSignature degree)

        SoftChromatic ->
            Martyria degree (softChromaticSignature degree)

        Enharmonic ->
            Martyria degree (enharmonicSignature degree)


diatonicSignature : Degree -> ModalSignature
diatonicSignature degree =
    case degree of
        GA ->
            NaNa

        DI ->
            Delta

        KE ->
            Alpha

        Zo ->
            Baris

        Ni ->
            Delta

        Pa ->
            Alpha

        Bou ->
            Legetos

        Ga ->
            NaNa

        Di ->
            DeltaDotted

        Ke ->
            AlphaDotted

        Zo_ ->
            Legetos

        Ni_ ->
            NaNa

        Pa_ ->
            AlphaDotted

        Bou_ ->
            Legetos

        Ga_ ->
            NaNa


enharmonicSignature : Degree -> ModalSignature
enharmonicSignature step =
    case step of
        Zo_ ->
            NaNa

        _ ->
            diatonicSignature step


hardChromaticSignature : Degree -> ModalSignature
hardChromaticSignature step =
    case step of
        GA ->
            NenanoHard

        DI ->
            BetaHard

        KE ->
            NenanoHard

        Zo ->
            BetaHard

        Ni ->
            NenanoHard

        Pa ->
            BetaHard

        Bou ->
            NenanoHard

        Ga ->
            BetaHard

        Di ->
            NenanoHard

        Ke ->
            BetaHard

        Zo_ ->
            NenanoHard

        Ni_ ->
            BetaHard

        Pa_ ->
            NenanoHard

        Bou_ ->
            BetaHard

        Ga_ ->
            NenanoHard


softChromaticSignature : Degree -> ModalSignature
softChromaticSignature step =
    case step of
        GA ->
            NenanoSoft

        DI ->
            NenanoSoft

        KE ->
            BetaSoft

        Zo ->
            NenanoSoft

        Ni ->
            BetaSoft

        Pa ->
            NenanoSoft

        Bou ->
            BetaSoft

        Ga ->
            NenanoSoft

        Di ->
            BetaSoft

        Ke ->
            NenanoSoft

        Zo_ ->
            BetaSoft

        Ni_ ->
            NenanoSoft

        Pa_ ->
            BetaSoft

        Bou_ ->
            NenanoSoft

        Ga_ ->
            BetaSoft
