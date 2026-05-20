module Byzantine.Mode.Signature exposing
    ( Signature
    , Ichos(..), Indicator(..)
    , Elements, elements
    , all
    , authenticOneKe, authenticOnePa
    , secondDi, secondPa, secondBou
    , lowerThirdGa_1, lowerThirdGa_2, lowerThirdGa_3, lowerThirdGa_papadic
    , authenticFourDi, medialFourBou_1, medialFourBou_2, paramedialFourPa, medialFourDi_softChromatic, medialFourBou_softChromatic
    , plagalFirstPa, plagalFirstKe, plagalFirstPa_pentaphone, plagalFirstPa_phrygian, plagalFirstPa_minor
    , plagalSecondPa, plagalSecondBou, plagalSecondDi_softChromatic, plagalSecondDi_hardChromatic
    , graveGa, graveZo, graveZoFlat
    , plagalFourthNi, plagalFourthGa_1, plagalFourthGa_2
    )

{-| WIP.

I think I'd like for Signature to be opaque to ensure validity, but for
view purposes, we'll need some way of deconstructing a signature into its
constituent parts. The signature itself as a type should be able to encode the
constituent elements, but I think probably not how they are rendered in all
cases.


# Types

@docs Signature
@docs Ichos, Indicator


## Elements

@docs Elements, elements


# Catalog

@docs all


## Authentic First

@docs authenticOneKe, authenticOnePa


## Authentic Second

@docs secondDi, secondPa, secondBou


## Authentic Third

@docs lowerThirdGa_1, lowerThirdGa_2, lowerThirdGa_3, lowerThirdGa_papadic


## Authentic Fourth

@docs authenticFourDi, medialFourBou_1, medialFourBou_2, paramedialFourPa, medialFourDi_softChromatic, medialFourBou_softChromatic


## Plagal First

@docs plagalFirstPa, plagalFirstKe, plagalFirstPa_pentaphone, plagalFirstPa_phrygian, plagalFirstPa_minor


## Plagal Second

@docs plagalSecondPa, plagalSecondBou, plagalSecondDi_softChromatic, plagalSecondDi_hardChromatic


## Plagal Third

@docs graveGa, graveZo, graveZoFlat


## Plagal Fourth

@docs plagalFourthNi, plagalFourthGa_1, plagalFourthGa_2

-}

import Byzantine.Degree exposing (Degree(..))
import Byzantine.Fthora as Fthora exposing (Fthora)
import Byzantine.IntervalCharacter as IntervalCharacter
    exposing
        ( AscendingChar(..)
        , DescendingChar(..)
        , IntervalCharacter(..)
        , SkipType(..)
        )
import Byzantine.Scale exposing (Scale(..))


{-| The majority of modal signatures have the same basic shape: the word
"Ichos", an optional plagal indicator, the classification indicator, and the
base (the degree, usually plus a fthora).

Some have a different pattern, which usually involves a neume indicator
(interval character).

-}
type Signature
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


elements : Signature -> Elements
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


all : List Signature
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


firstModes : List Signature
firstModes =
    [ authenticOneKe, authenticOnePa ]


authenticOneKe : Signature
authenticOneKe =
    Regular
        { ichos = Ichos
        , indicator = First
        , base = Base Ke (Fthora.for Diatonic Ke)
        }


authenticOnePa : Signature
authenticOnePa =
    Regular
        { ichos = Ichos
        , indicator = First
        , base = Base Pa (Fthora.for Diatonic Pa)
        }



-- Mode 2


secondModes : List Signature
secondModes =
    [ secondDi, secondPa, secondBou ]


secondDi : Signature
secondDi =
    Regular
        { ichos = Ichos
        , indicator = Second
        , base = Base Di (Fthora.for SoftChromatic Di)
        }


secondPa : Signature
secondPa =
    Regular
        { ichos = Ichos
        , indicator = Second
        , base = Base Pa (Fthora.for HardChromatic Pa)
        }


secondBou : Signature
secondBou =
    Regular
        { ichos = Ichos
        , indicator = Second
        , base = Base Bou (Fthora.for HardChromatic Pa)
        }



-- Mode 3


thirdModes : List Signature
thirdModes =
    [ lowerThirdGa_1, lowerThirdGa_2, lowerThirdGa_3, lowerThirdGa_papadic ]


{-| TODO: think on this. Should be something like "Ηχος γ. Γα"
-}
lowerThirdGa_1 : Signature
lowerThirdGa_1 =
    Irregular
        { ichos = Ichos
        , indicator = Nothing
        , baseDegree = Just Ga
        , fthora = Fthora.for Diatonic Ga
        , neume = Nothing
        }


lowerThirdGa_2 : Signature
lowerThirdGa_2 =
    Regular
        { ichos = Ichos
        , indicator = Third
        , base = Base Ga (Fthora.for Diatonic Ga)
        }


lowerThirdGa_3 : Signature
lowerThirdGa_3 =
    Regular
        { ichos = Ichos
        , indicator = ThirdNaNa
        , base = Base Ga (Fthora.for Diatonic Ga)
        }


lowerThirdGa_papadic : Signature
lowerThirdGa_papadic =
    Regular
        { ichos = Ichos
        , indicator = ThirdNaNa
        , base = Base Ga (Fthora.for Diatonic Ni)
        }



-- Mode 4


fourthModes : List Signature
fourthModes =
    [ authenticFourDi
    , medialFourBou_1
    , medialFourBou_2
    , paramedialFourPa
    , medialFourDi_softChromatic
    , medialFourBou_softChromatic
    ]


authenticFourDi : Signature
authenticFourDi =
    Regular
        { ichos = Ichos
        , indicator = Fourth
        , base = Base Di (Fthora.for Diatonic Di)
        }


medialFourBou_1 : Signature
medialFourBou_1 =
    Irregular
        { ichos = Ichos
        , indicator = Just Fourth
        , baseDegree = Just Bou
        , fthora = Fthora.for Diatonic Bou
        , neume = Just (Descending SynechesElafron Nothing)
        }


medialFourBou_2 : Signature
medialFourBou_2 =
    Irregular
        { ichos = Ichos
        , indicator = Just Legetos
        , baseDegree = Just Bou
        , fthora = Fthora.for Diatonic Bou
        , neume = Nothing
        }


paramedialFourPa : Signature
paramedialFourPa =
    Regular
        { ichos = Ichos
        , indicator = Fourth
        , base = Base Pa (Fthora.for Diatonic Pa)
        }


medialFourDi_softChromatic : Signature
medialFourDi_softChromatic =
    Regular
        { ichos = Ichos
        , indicator = Fourth
        , base = Base Di (Fthora.for SoftChromatic Di)
        }


medialFourBou_softChromatic : Signature
medialFourBou_softChromatic =
    Irregular
        { ichos = Ichos
        , indicator = Just Fourth
        , baseDegree = Just Bou
        , fthora = Fthora.for SoftChromatic Bou
        , neume = Just (Descending SynechesElafron Nothing)
        }



-- Plagal Mode 1


plagalFirstModes : List Signature
plagalFirstModes =
    [ plagalFirstPa
    , plagalFirstKe
    , plagalFirstPa_pentaphone
    , plagalFirstPa_phrygian
    , plagalFirstPa_minor
    ]


plagalFirstPa : Signature
plagalFirstPa =
    Regular
        { ichos = IchosPlagal
        , indicator = PlagalFirst
        , base = Base Pa (Fthora.for Diatonic Pa)
        }


plagalFirstKe : Signature
plagalFirstKe =
    Irregular
        { ichos = IchosPlagal
        , indicator = Just PlagalFirst
        , baseDegree = Just Ke
        , fthora = Fthora.for Diatonic Ke
        , neume = IntervalCharacter.basicInterval 5
        }


plagalFirstPa_pentaphone : Signature
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
plagalFirstPa_phrygian : Signature
plagalFirstPa_phrygian =
    Irregular
        { ichos = IchosPlagal
        , indicator = Just PlagalFirst
        , baseDegree = Just Pa
        , fthora = Nothing
        , neume = Nothing
        }


plagalFirstPa_minor : Signature
plagalFirstPa_minor =
    Regular
        { ichos = IchosPlagal
        , indicator = PlagalFirst
        , base = Base Pa (Fthora.for Diatonic Di)
        }



-- Plagal Mode 2


plagalSecondModes : List Signature
plagalSecondModes =
    [ plagalSecondPa
    , plagalSecondBou
    , plagalSecondDi_softChromatic
    , plagalSecondDi_hardChromatic
    ]


plagalSecondPa : Signature
plagalSecondPa =
    Regular
        { ichos = IchosPlagal
        , indicator = PlagalSecond
        , base = Base Pa (Fthora.for HardChromatic Pa)
        }


plagalSecondBou : Signature
plagalSecondBou =
    Irregular
        { ichos = IchosPlagal
        , indicator = Just PlagalSecond
        , baseDegree = Just Bou
        , fthora = Fthora.for SoftChromatic Di
        , neume = Just (Ascending (Skip OligonKentimaBelow) Nothing)
        }


plagalSecondDi_softChromatic : Signature
plagalSecondDi_softChromatic =
    Regular
        { ichos = IchosPlagal
        , indicator = PlagalSecond
        , base = Base Di (Fthora.for SoftChromatic Di)
        }


plagalSecondDi_hardChromatic : Signature
plagalSecondDi_hardChromatic =
    Irregular
        { ichos = IchosPlagal
        , indicator = Just PlagalSecond
        , baseDegree = Just Di
        , fthora = Fthora.for HardChromatic Di
        , neume = IntervalCharacter.basicInterval 3
        }



-- Plagal Mode 3


graveModes : List Signature
graveModes =
    [ graveGa, graveZo, graveZoFlat ]


graveGa : Signature
graveGa =
    Regular
        { ichos = Ichos
        , indicator = Varys
        , base = Base Ga (Fthora.for Diatonic Ga)
        }


graveZo : Signature
graveZo =
    Regular
        { ichos = Ichos
        , indicator = Varys
        , base = Base Zo (Fthora.for Diatonic Zo)
        }


graveZoFlat : Signature
graveZoFlat =
    Regular
        { ichos = Ichos
        , indicator = Varys
        , base = Base Zo (Fthora.for Enharmonic Zo_)
        }



-- Plagal Mode 4


plagalFourthModes : List Signature
plagalFourthModes =
    [ plagalFourthNi, plagalFourthGa_1, plagalFourthGa_2 ]


plagalFourthNi : Signature
plagalFourthNi =
    Regular
        { ichos = IchosPlagal
        , indicator = PlagalFourth
        , base = Base Ni (Fthora.for Diatonic Ni)
        }


plagalFourthGa_1 : Signature
plagalFourthGa_1 =
    Irregular
        { ichos = IchosPlagal
        , indicator = Just PlagalFourth
        , baseDegree = Nothing
        , fthora = Nothing
        , neume = IntervalCharacter.basicInterval 4
        }


plagalFourthGa_2 : Signature
plagalFourthGa_2 =
    Regular
        { ichos = IchosPlagal
        , indicator = PlagalFourth
        , base = Base Ga (Fthora.for Diatonic Ni)
        }
