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
    | AuthenticOneEirmologic
    | AuthenticOneSticheraric
    | PlagalOneSticheraric
    | PlagalOnePapadic


{-| Display string.
-}
toString : Mode -> String
toString mode =
    case mode of
        AuthenticOnePapadic ->
            "Authentic Mode One, Papadic"

        AuthenticOneEirmologic ->
            "Lower Mode One, Eirmologic"

        AuthenticOneSticheraric ->
            "Lower Mode One, Sticheraric"

        PlagalOneSticheraric ->
            "Plagal Mode One, Sticheraric"

        PlagalOnePapadic ->
            "Plagal Mode One, Papadic"


all : List Mode
all =
    let
        next : List Mode -> List Mode
        next modes =
            case List.head modes of
                Nothing ->
                    AuthenticOnePapadic :: modes |> next

                Just AuthenticOnePapadic ->
                    AuthenticOneEirmologic :: modes |> next

                Just AuthenticOneEirmologic ->
                    AuthenticOneSticheraric :: modes |> next

                Just AuthenticOneSticheraric ->
                    PlagalOneSticheraric :: modes |> next

                Just PlagalOneSticheraric ->
                    PlagalOnePapadic :: modes |> next

                Just PlagalOnePapadic ->
                    modes
    in
    next [] |> List.reverse



-- MODE DATA


data : Mode -> ModeData
data mode =
    case mode of
        AuthenticOnePapadic ->
            authenticOnePapadic

        AuthenticOneEirmologic ->
            authenticOneEirmologic

        AuthenticOneSticheraric ->
            authenticOneSticheraric

        PlagalOneSticheraric ->
            plagalOneSticheraric

        PlagalOnePapadic ->
            plagalOnePapadic


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


authenticOneEirmologic : ModeData
authenticOneEirmologic =
    { scale = Diatonic
    , dominantTones =
        { base = Pa
        , cadencePoints =
            { final = Pa
            , complete = [ Pa ]
            , medial = []
            , incomplete = [ Ga, Di ]
            }
        , nonCadentialFoci = [ Ke ]
        }
    , isonOptions = [ Pa, Di ]
    , range =
        { start = KE
        , end = Pa_
        }
    , recitingTone = Di
    , possibleInflections =
        [ { degree = Ga, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Bou, accidentals = [ Sharp2 ] }
        , { degree = Di, accidentals = [ Flat4 ] }
        , { degree = Zo_, accidentals = [ Flat4 ] }
        ]
    }


{-| Lower first, includes a papadic genre that has a few differences
with dominant tones.

Ke as non-cadential melodic focus

-}
authenticOneSticheraric : ModeData
authenticOneSticheraric =
    { scale = Diatonic
    , dominantTones =
        { base = Pa
        , cadencePoints =
            { final = Pa
            , complete = [ Pa ]
            , medial = []
            , incomplete = [ Pa, Ga, Di ]
            }
        , nonCadentialFoci = []
        }
    , isonOptions = [ Pa, DI ]
    , range = { start = DI, end = Pa_ }
    , recitingTone = Pa
    , possibleInflections =
        [ { degree = Bou, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Di, accidentals = [ Flat2, Flat4, Flat6 ] }
        , { degree = Ga, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Zo_, accidentals = [ Flat4 ] }
        ]
    }


{-| papadic will be basically the same but for some cadence points

There are two variants: fast sticheraric will have final of Ke.

  - TODO: verify this is the right accidental for Zo

-}
plagalOneSticheraric : ModeData
plagalOneSticheraric =
    { scale = Diatonic
    , dominantTones =
        { base = Pa
        , cadencePoints =
            { final = Di
            , complete = [ Di ]
            , medial = []
            , incomplete = [ Ke ]
            }
        , nonCadentialFoci = []
        }
    , isonOptions = [ Pa, Ke, Di, KE ]
    , range =
        { start = KE
        , end = Pa_
        }
    , recitingTone = Di
    , possibleInflections =
        [ { degree = Ga, accidentals = [ Sharp2, Sharp4, Sharp6 ] }
        , { degree = Zo_, accidentals = [ Flat4 ] }
        ]
    }


{-| papadic will be basically the same but for some cadence points

There are two variants: fast sticheraric will have final of Ke.

  - TODO: verify this is the right accidental for Zo
  - TODO: additional details for papadic

-}
plagalOnePapadic : ModeData
plagalOnePapadic =
    { scale = Diatonic
    , dominantTones =
        { base = Pa
        , cadencePoints =
            { final = Pa
            , complete = [ Di ]
            , medial = []
            , incomplete = [ Ga, Ke ]
            }
        , nonCadentialFoci = []
        }
    , isonOptions = [ Pa, Ke, Di, KE ]
    , range =
        { start = KE
        , end = Pa_
        }
    , recitingTone = Di
    , possibleInflections =
        [ { degree = Bou, accidentals = [ Sharp2 ] }
        , { degree = Ga, accidentals = [ Sharp2, Sharp4, Sharp6 ] }
        , { degree = Di, accidentals = [ Flat4 ] }
        , { degree = Zo_, accidentals = [ Flat4 ] }
        ]
    }
