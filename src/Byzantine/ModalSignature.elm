module Byzantine.ModalSignature exposing
    ( ModalSignature
    , Ichos(..), Indicator(..)
    , Elements, elements
    , all
    , authenticOneKe
    , authenticFourDi, medialFourBou_1
    )

{-| WIP.

I think I'd like for ModalSignature to be opaque to ensure validity, but for
view purposes, we'll need some way of deconstructing a signature into its
constituent parts. The signature itself as a type should be able to encode the
constituent elements, but I think probably not how they are rendered in all
cases.


# Types

@docs ModalSignature
@docs Ichos, Indicator


## Elements

@docs Elements, elements


# Catalog

@docs all


## Authentic First

@docs authenticOneKe


## Authentic Second


## Authentic Third


## Authentic Fourth

@docs authenticFourDi, medialFourBou_1


## Plagal First


## Plagal Second


## Plagal Third


## Plagal Fourth

-}

import Byzantine.Degree exposing (Degree(..))
import Byzantine.Fthora as Fthora exposing (Fthora)
import Byzantine.IntervalCharacter as IntervalCharacter
    exposing
        ( AscendingChar(..)
        , AscendingSteps(..)
        , DescendingChar(..)
        , IntervalCharacter(..)
        , SkipType(..)
        , SomaChar(..)
        , basicInterval
        )
import Byzantine.Scale exposing (Scale(..))


{-| The majority of modal signatures have the same basic shape: the word
"Ichos", an optional plagal indicator, the classification indicator, and the
base (the degree, usually plus a fthora).

Some have a different pattern, which usually involves a neume indicator
(interval character).

-}
type ModalSignature
    = Regular
        { ichos : Ichos
        , indicator : Indicator
        , base : Base
        }
    | Irregular Elements


type alias Elements =
    { ichos : Ichos
    , indicator : Maybe Indicator
    , baseDegree : Maybe Degree
    , fthora : Maybe Fthora
    , neume : Maybe IntervalCharacter
    }


elements : ModalSignature -> Elements
elements signature =
    case signature of
        Regular { ichos, indicator, base } ->
            let
                { degree, fthora } =
                    deconstructBase base
            in
            { ichos = ichos
            , indicator = Just indicator
            , baseDegree = Just degree
            , fthora = fthora
            , neume = Nothing
            }

        Irregular elements_ ->
            elements_


type Ichos
    = Ichos
    | IchosPlagal


{-| Generally evolved as an abbreviation of the apichima.
-}
type Indicator
    = First
    | Second
    | Third
    | ThirdNaNa
    | Fourth
    | Legetos
    | PlagalFirst
    | PlagalSecond
    | Varys
    | VarysZo
    | PlagalFourth


{-| Regular base could probably be modeled as the syllable plus a fthora. We'll
need to make a catalog of these, though.

Generally, the fthora will be a natural correspondence to the base, but not
always. E.g., Plagal 4 eirologic is sung from Ga, but with a fthora af Ni. Same
with lower third mode from Ga.

Wrinkle: there won't _always_ be a fthora. E.g., Grave mode from Zo. There is no
traditional fthora for Zo in base position. So this will need to be a Maybe.

Another wrinkle: while this should be sufficient to conceptually model a regular
base from a normalized data perspective, it won't necessarily capture
traditional presentation.

-}
type Base
    = Base Degree (Maybe Fthora)


deconstructBase : Base -> { degree : Degree, fthora : Maybe Fthora }
deconstructBase (Base degree fthora) =
    { degree = degree, fthora = fthora }



-- SIGNATURES


all : List ModalSignature
all =
    List.concat
        [ firstModes
        , secondModes
        , thirdModes
        , fourthModes
        , plagalFirstModes
        , plagalSecondModes
        , graveModes
        , plagalFourthModes
        ]



-- Mode 1


firstModes : List ModalSignature
firstModes =
    [ authenticOneKe, authenticOnePa ]


authenticOneKe : ModalSignature
authenticOneKe =
    Regular
        { ichos = Ichos
        , indicator = First
        , base = Base Ke (Fthora.for Diatonic Ke)
        }


authenticOnePa : ModalSignature
authenticOnePa =
    Regular
        { ichos = Ichos
        , indicator = First
        , base = Base Pa (Fthora.for Diatonic Pa)
        }



-- Mode 2


secondModes : List ModalSignature
secondModes =
    [ secondDi, secondPa, secondBou ]


secondDi : ModalSignature
secondDi =
    Regular
        { ichos = Ichos
        , indicator = Second
        , base = Base Di (Fthora.for SoftChromatic Di)
        }


secondPa : ModalSignature
secondPa =
    Regular
        { ichos = Ichos
        , indicator = Second
        , base = Base Pa (Fthora.for HardChromatic Pa)
        }


secondBou : ModalSignature
secondBou =
    Regular
        { ichos = Ichos
        , indicator = Second
        , base = Base Bou (Fthora.for HardChromatic Pa)
        }



-- Mode 3


thirdModes : List ModalSignature
thirdModes =
    [ lowerThirdGa_1, lowerThirdGa_2, lowerThirdGa_3, lowerThirdGa_papadic ]


{-| TODO: think on this. Should be something like "Ηχος γ. Γα"
-}
lowerThirdGa_1 : ModalSignature
lowerThirdGa_1 =
    Irregular
        { ichos = Ichos
        , indicator = Nothing
        , baseDegree = Just Ga
        , fthora = Fthora.for Diatonic Ga
        , neume = Nothing
        }


lowerThirdGa_2 : ModalSignature
lowerThirdGa_2 =
    Regular
        { ichos = Ichos
        , indicator = Third
        , base = Base Ga (Fthora.for Diatonic Ga)
        }


lowerThirdGa_3 : ModalSignature
lowerThirdGa_3 =
    Regular
        { ichos = Ichos
        , indicator = ThirdNaNa
        , base = Base Ga (Fthora.for Diatonic Ga)
        }


lowerThirdGa_papadic : ModalSignature
lowerThirdGa_papadic =
    Regular
        { ichos = Ichos
        , indicator = ThirdNaNa
        , base = Base Ga (Fthora.for Diatonic Ni)
        }



-- Mode 4


fourthModes : List ModalSignature
fourthModes =
    [ authenticFourDi
    , medialFourBou_1
    , medialFourBou_2
    , paramedialFourPa
    , medialFourDi_softChromatic
    , medialFourBou_softChromatic
    ]


authenticFourDi : ModalSignature
authenticFourDi =
    Regular
        { ichos = Ichos
        , indicator = Fourth
        , base = Base Di (Fthora.for Diatonic Di)
        }


medialFourBou_1 : ModalSignature
medialFourBou_1 =
    Irregular
        { ichos = Ichos
        , indicator = Just Fourth
        , baseDegree = Just Bou
        , fthora = Fthora.for Diatonic Bou
        , neume = Just (Descending SynechesElafron Nothing)
        }


medialFourBou_2 : ModalSignature
medialFourBou_2 =
    Irregular
        { ichos = Ichos
        , indicator = Just Legetos
        , baseDegree = Just Bou
        , fthora = Fthora.for Diatonic Bou
        , neume = Nothing
        }


paramedialFourPa : ModalSignature
paramedialFourPa =
    Regular
        { ichos = Ichos
        , indicator = Fourth
        , base = Base Pa (Fthora.for Diatonic Pa)
        }


medialFourDi_softChromatic : ModalSignature
medialFourDi_softChromatic =
    Regular
        { ichos = Ichos
        , indicator = Fourth
        , base = Base Di (Fthora.for SoftChromatic Di)
        }


medialFourBou_softChromatic : ModalSignature
medialFourBou_softChromatic =
    Irregular
        { ichos = Ichos
        , indicator = Just Fourth
        , baseDegree = Just Bou
        , fthora = Fthora.for SoftChromatic Bou
        , neume = Just (Descending SynechesElafron Nothing)
        }



-- Plagal Mode 1


plagalFirstModes : List ModalSignature
plagalFirstModes =
    [ plagalFirstPa
    , plagalFirstKe
    , plagalFirstPa_pentaphone
    , plagalFirstPa_phrygian
    , plagalFirstPa_minor
    ]


plagalFirstPa : ModalSignature
plagalFirstPa =
    Regular
        { ichos = IchosPlagal
        , indicator = PlagalFirst
        , base = Base Pa (Fthora.for Diatonic Pa)
        }


plagalFirstKe : ModalSignature
plagalFirstKe =
    Irregular
        { ichos = IchosPlagal
        , indicator = Just PlagalFirst
        , baseDegree = Just Ke
        , fthora = Fthora.for Diatonic Ke
        , neume = IntervalCharacter.basicInterval 5
        }


plagalFirstPa_pentaphone : ModalSignature
plagalFirstPa_pentaphone =
    Irregular
        { ichos = IchosPlagal
        , indicator = Just PlagalFirst
        , baseDegree = Just Pa
        , fthora = Fthora.for Enharmonic Zo_
        , neume = IntervalCharacter.basicInterval 5
        }


{-| TODO: I think there's a gap in the data modeling. yfesis-geniki.
This may be something to incorporate into the Accidental modeling
(local vs persistent).
-}
plagalFirstPa_phrygian : ModalSignature
plagalFirstPa_phrygian =
    Irregular
        { ichos = IchosPlagal
        , indicator = Just PlagalFirst
        , baseDegree = Just Pa
        , fthora = Nothing
        , neume = Nothing
        }


plagalFirstPa_minor : ModalSignature
plagalFirstPa_minor =
    Regular
        { ichos = IchosPlagal
        , indicator = PlagalFirst
        , base = Base Pa (Fthora.for Diatonic Di)
        }



-- Plagal Mode 2


plagalSecondModes : List ModalSignature
plagalSecondModes =
    [ plagalSecondPa
    , plagalSecondBou
    , plagalSecondDi_softChromatic
    , plagalSecondDi_hardChromatic
    ]


plagalSecondPa : ModalSignature
plagalSecondPa =
    Regular
        { ichos = IchosPlagal
        , indicator = PlagalSecond
        , base = Base Pa (Fthora.for HardChromatic Pa)
        }


plagalSecondBou : ModalSignature
plagalSecondBou =
    Irregular
        { ichos = IchosPlagal
        , indicator = Just PlagalSecond
        , baseDegree = Just Bou
        , fthora = Fthora.for SoftChromatic Di
        , neume = Just (Ascending (Skip OligonKentimaBelow) Nothing)
        }


plagalSecondDi_softChromatic : ModalSignature
plagalSecondDi_softChromatic =
    Regular
        { ichos = IchosPlagal
        , indicator = PlagalSecond
        , base = Base Di (Fthora.for SoftChromatic Di)
        }


plagalSecondDi_hardChromatic : ModalSignature
plagalSecondDi_hardChromatic =
    Irregular
        { ichos = IchosPlagal
        , indicator = Just PlagalSecond
        , baseDegree = Just Di
        , fthora = Fthora.for HardChromatic Di
        , neume = IntervalCharacter.basicInterval 3
        }



-- Plagal Mode 3


graveModes : List ModalSignature
graveModes =
    [ graveGa, graveZo, graveZoFlat ]


graveGa : ModalSignature
graveGa =
    Regular
        { ichos = Ichos
        , indicator = Varys
        , base = Base Ga (Fthora.for Diatonic Ga)
        }


graveZo : ModalSignature
graveZo =
    Regular
        { ichos = Ichos
        , indicator = Varys
        , base = Base Zo (Fthora.for Diatonic Zo)
        }


graveZoFlat : ModalSignature
graveZoFlat =
    Regular
        { ichos = Ichos
        , indicator = Varys
        , base = Base Zo (Fthora.for Enharmonic Zo_)
        }



-- Plagal Mode 4


plagalFourthModes : List ModalSignature
plagalFourthModes =
    [ plagalFourthNi, plagalFourthGa_1, plagalFourthGa_2 ]


plagalFourthNi : ModalSignature
plagalFourthNi =
    Regular
        { ichos = IchosPlagal
        , indicator = PlagalFourth
        , base = Base Ni (Fthora.for Diatonic Ni)
        }


plagalFourthGa_1 : ModalSignature
plagalFourthGa_1 =
    Irregular
        { ichos = IchosPlagal
        , indicator = Just PlagalFourth
        , baseDegree = Nothing
        , fthora = Nothing
        , neume = IntervalCharacter.basicInterval 4
        }


plagalFourthGa_2 : ModalSignature
plagalFourthGa_2 =
    Regular
        { ichos = IchosPlagal
        , indicator = PlagalFourth
        , base = Base Ga (Fthora.for Diatonic Ni)
        }
