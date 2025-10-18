module Byzantine.Mode exposing
    ( Mode(..), toString, all
    , ModeData, data
    , Inflection
    )

{-| Data structure to capture modal characteristics.


# Mode

@docs Mode, toString, all


# Mode Data

@docs ModeData, data


## Melodic Attractions

@docs Inflection

-}

import Byzantine.Accidental exposing (Accidental(..))
import Byzantine.Degree exposing (Degree(..))
import Byzantine.Scale exposing (Scale(..))


{-| The data modeling here is still mostly TBD.
-}
type Mode
    = AuthenticOnePapadic


{-| Display string.
-}
toString : Mode -> String
toString mode =
    case mode of
        AuthenticOnePapadic ->
            "Authentic Mode One, Papadic"


all : List Mode
all =
    [ AuthenticOnePapadic ]



-- MODE DATA


data : Mode -> ModeData
data mode =
    case mode of
        AuthenticOnePapadic ->
            authenticOnePapadic


{-| other points to capture:

  - ison (may also want to include "unison")
  - modal signature
  - apichima

-}
type alias ModeData =
    { scale : Scale
    , dominantTones : DominantTones
    , isonOptions : List Degree
    , range :
        { start : Degree
        , end : Degree
        }
    , recitingTone : Degree
    , possibleInflections : List Inflection
    }


type alias DominantTones =
    { base : Degree
    , cadencePoints : CadencePoints
    , nonCadentialFoci : List Degree
    }


type alias CadencePoints =
    { final : Degree
    , complete : List Degree
    , medial : List Degree
    , incomplete : List Degree
    }


{-| This will need to be expanded to capture additional musical details, namely,
context, and whether or not the inflection is discretionary or forced.
-}
type alias Inflection =
    { degree : Degree
    , accidentals : List Accidental
    }



-- CATALOG


authenticOnePapadic : ModeData
authenticOnePapadic =
    { scale = Diatonic
    , dominantTones =
        { base = Ke
        , cadencePoints =
            { final = Ke
            , complete = [ Ke, Pa ]
            , medial = [ Ke, Ni_, Pa_ ]
            , incomplete = [ Ke, Pa_ ]
            }
        , nonCadentialFoci = []
        }
    , isonOptions = [ Ke ]
    , range =
        { start = Pa
        , end = Pa_
        }
    , recitingTone = Ke
    , possibleInflections =
        [ { degree = Zo_, accidentals = [ Sharp2 ] }
        , { degree = Pa_, accidentals = [ Flat2, Flat4 ] }
        , { degree = Ni_, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Ga, accidentals = [ Sharp4, Sharp6 ] }
        ]
    }
