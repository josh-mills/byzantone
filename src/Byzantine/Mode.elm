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
import Byzantine.Mode.Classification exposing (Classification(..), Division(..), Ordinal(..))
import Byzantine.Scale exposing (Scale(..))


{-| The data modeling here is still mostly TBD.

We'll need to pick up Hard-Diatonic Aneanes modes (categorized as plagal first)

-}
type Mode
    = AuthenticOne_Ke_Papadic
    | AuthenticOne_Pa_Eirmologic
    | AuthenticOne_Pa_Sticheraric
    | PlagalOne_Pa_Sticheraric
    | PlagalOne_Pa_Papadic
    | PlagalOne_Ke_Eirmologic


{-| Display string.
-}
toString : Mode -> String
toString mode =
    case mode of
        AuthenticOne_Ke_Papadic ->
            "Authentic Mode One, Papadic"

        AuthenticOne_Pa_Eirmologic ->
            "Lower Mode One, Eirmologic"

        AuthenticOne_Pa_Sticheraric ->
            "Lower Mode One, Sticheraric"

        PlagalOne_Pa_Sticheraric ->
            "Plagal Mode One, Sticheraric"

        PlagalOne_Pa_Papadic ->
            "Plagal Mode One, Papadic"

        PlagalOne_Ke_Eirmologic ->
            "Plagal Mode One, Eirmologic"


all : List Mode
all =
    let
        next : List Mode -> List Mode
        next modes =
            case List.head modes of
                Nothing ->
                    AuthenticOne_Ke_Papadic :: modes |> next

                Just AuthenticOne_Ke_Papadic ->
                    AuthenticOne_Pa_Eirmologic :: modes |> next

                Just AuthenticOne_Pa_Eirmologic ->
                    AuthenticOne_Pa_Sticheraric :: modes |> next

                Just AuthenticOne_Pa_Sticheraric ->
                    PlagalOne_Pa_Sticheraric :: modes |> next

                Just PlagalOne_Pa_Sticheraric ->
                    PlagalOne_Pa_Papadic :: modes |> next

                Just PlagalOne_Pa_Papadic ->
                    PlagalOne_Ke_Eirmologic :: modes |> next

                Just PlagalOne_Ke_Eirmologic ->
                    modes
    in
    next [] |> List.reverse



-- MODE DATA


data : Mode -> ModeData
data mode =
    case mode of
        AuthenticOne_Ke_Papadic ->
            authenticOne_Ke_Papadic

        AuthenticOne_Pa_Eirmologic ->
            authenticOne_Pa_Eirmologic

        AuthenticOne_Pa_Sticheraric ->
            authenticOne_Pa_Sticheraric

        PlagalOne_Pa_Sticheraric ->
            plagalOne_Pa_Sticheraric

        PlagalOne_Pa_Papadic ->
            plagalOne_Pa_Papadic

        PlagalOne_Ke_Eirmologic ->
            plagalOne_Ke_Eirmologic


{-| other points to capture:

  - ison (may also want to include "unison")
  - modal signature (may be more than one)
  - apichima

-}
type alias ModeData =
    { classification : Classification
    , scale : Scale
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


authenticOne_Ke_Papadic : ModeData
authenticOne_Ke_Papadic =
    { classification = Classification Authentic ModeOne
    , scale = Diatonic
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


authenticOne_Pa_Eirmologic : ModeData
authenticOne_Pa_Eirmologic =
    { classification = Classification Authentic ModeOne
    , scale = Diatonic
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
authenticOne_Pa_Sticheraric : ModeData
authenticOne_Pa_Sticheraric =
    { classification = Classification Authentic ModeOne
    , scale = Diatonic
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
plagalOne_Pa_Sticheraric : ModeData
plagalOne_Pa_Sticheraric =
    { classification = Classification Plagal ModeOne
    , scale = Diatonic
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
plagalOne_Pa_Papadic : ModeData
plagalOne_Pa_Papadic =
    { classification = Classification Plagal ModeOne
    , scale = Diatonic
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


plagalOne_Ke_Eirmologic : ModeData
plagalOne_Ke_Eirmologic =
    { classification = Classification Plagal ModeOne
    , scale = Diatonic
    , dominantTones =
        { base = Ke
        , cadencePoints =
            { final = Ke
            , complete = [ Ke ]
            , medial = []
            , incomplete = [ Ni_ ]
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
        , { degree = Ga_, accidentals = [ Flat4 ] }
        ]
    }
