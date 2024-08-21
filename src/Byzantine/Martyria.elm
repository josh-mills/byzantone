module Byzantine.Martyria exposing
    ( Martyria
    , ModalSignature(..)
    , degree
    , for
    , signature
    , unwrap
    )

{-| The μαρτυρἰαι ("testimonies"), or signatures, of Byzantine music,
representing a note within the context of a given modal context.
-}

import Byzantine.Scale exposing (Degree(..), Scale(..))


{-| The μαρτυρἰαι ("testimonies"), or signatures, of Byzantine music,
representing a note within the context of a given modal context.

Because a martyria can be derived from the scale and the scale degree, the type
is opaque so that only valid combinations can be created.

-}
type Martyria
    = Martyria Degree ModalSignature


degree : Martyria -> Degree
degree (Martyria degree_ _) =
    degree_


signature : Martyria -> ModalSignature
signature (Martyria _ signature_) =
    signature_


unwrap : Martyria -> ( Degree, ModalSignature )
unwrap (Martyria degree_ signature_) =
    ( degree_, signature_ )


{-| A modal signature (μαπτυρικὀν σημείον, "testimonal sign") is a shorthand
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
for scale step =
    case scale of
        Diatonic ->
            Martyria step (diatonicSignature step)

        HardChromatic ->
            Martyria step (hardChromaticSignature step)

        SoftChromatic ->
            Martyria step (softChromaticSignature step)

        Enharmonic ->
            Martyria step (enharmonicSignature step)


diatonicSignature : Degree -> ModalSignature
diatonicSignature step =
    case step of
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
