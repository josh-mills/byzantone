module Byzantine.Mode exposing
    ( Mode(..)
    , ModeData, data
    )

{-| Data structure to capture modal characteristics.


# Mode

@docs Mode


# Mode Data

@docs ModeData, data

-}

import Byzantine.Degree exposing (Degree(..))
import Byzantine.Scale exposing (Scale(..))


{-| The data modeling here is still mostly TBD.
-}
type Mode
    = AuthenticOnePapadic



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
    , rangeStart : Degree
    , rangeEnd : Degree
    , recitingTone : Degree
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
    , rangeStart = Pa
    , rangeEnd = Pa_
    , recitingTone = Ke
    }
