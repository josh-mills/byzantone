module ModeBuilder exposing (Model, Msg, init, update, view)

{-| Experimental modeling for constructing a mode.
-}

import Byzantine.Accidental as Accidental exposing (Accidental(..))
import Byzantine.Degree as Degree exposing (Degree(..))
import Byzantine.Pitch as Pitch exposing (Pitch)
import Byzantine.Scale as Scale exposing (Scale(..))
import Components.RadioFieldset as RadioFieldset
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Extra exposing (viewIf)
import Maybe.Extra
import Styles



-- MODEL


type alias Model =
    { base : Maybe Pitch
    , classification : Maybe Classification
    , scale : Maybe Scale
    , strategy : Strategy
    }


init : Model
init =
    { base = Nothing
    , classification = Nothing
    , scale = Nothing
    , strategy = SelectByScale
    }


type Strategy
    = SelectByScale
    | SelectByClassification
    | SelectBySignature


strategyToString : Strategy -> String
strategyToString strategy =
    case strategy of
        SelectByScale ->
            "By Scale"

        SelectByClassification ->
            "By Classification"

        SelectBySignature ->
            "By Signature"


strategyRadioConfig : RadioFieldset.Config Strategy Msg
strategyRadioConfig =
    RadioFieldset.baseConfig
        { itemToString = strategyToString
        , legendText = "Selection Strategy"
        , onSelect = SelectStrategy
        , options = [ SelectByScale, SelectByClassification, SelectBySignature ]
        }



-- MSG


type Msg
    = SelectBase (Maybe Pitch)
    | SelectClassification (Maybe Classification)
    | SelectScale (Maybe Scale)
    | SelectStrategy Strategy



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectBase maybeBase ->
            ( { model | base = maybeBase }, Cmd.none )

        SelectClassification maybeClassification ->
            ( { model | classification = maybeClassification, base = Nothing }, Cmd.none )

        SelectScale maybeScale ->
            ( { model | scale = maybeScale }, Cmd.none )

        SelectStrategy strategy ->
            ( { model | strategy = strategy }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ Styles.flexRow, class "p-4 gap-2" ]
        [ RadioFieldset.view strategyRadioConfig model.strategy
        , viewIf (model.strategy == SelectByScale)
            (RadioFieldset.view scaleRadioConfig model.scale)
        , viewIf (model.strategy == SelectByClassification)
            (RadioFieldset.view classificationRadioConfig model.classification)
        , Html.Extra.viewMaybe
            (\classification ->
                RadioFieldset.view
                    (baseRadioConfig (basesForClassification classification))
                    model.base
            )
            model.classification
        ]


scaleRadioConfig : RadioFieldset.Config (Maybe Scale) Msg
scaleRadioConfig =
    RadioFieldset.baseConfig
        { itemToString = Maybe.Extra.unwrap "" Scale.name >> (++) "mode-builder-"
        , legendText = "Scale"
        , onSelect = SelectScale
        , options = List.map Just Scale.all
        }
        |> RadioFieldset.withCustomViewItem
            (Html.Extra.viewMaybe text << Maybe.map Scale.name)


classificationRadioConfig : RadioFieldset.Config (Maybe Classification) Msg
classificationRadioConfig =
    RadioFieldset.baseConfig
        { itemToString = Maybe.Extra.unwrap "" toString >> (++) "mode-builder-"
        , legendText = "Classification"
        , onSelect = SelectClassification
        , options = List.map Just allClassifications
        }
        |> RadioFieldset.withCustomViewItem
            (Html.Extra.viewMaybe text << Maybe.map toString)


baseRadioConfig : List Pitch -> RadioFieldset.Config (Maybe Pitch) Msg
baseRadioConfig bases =
    RadioFieldset.baseConfig
        { itemToString = Maybe.Extra.unwrap "" Pitch.toString >> (++) "mode-builder-"
        , legendText = "Base"
        , onSelect = SelectBase
        , options = List.map Just bases
        }
        |> RadioFieldset.withCustomViewItem
            (Html.Extra.viewMaybe viewPitch)
        |> RadioFieldset.withCustomSelected
            (\option selected ->
                case ( option, selected ) of
                    ( Just o, Just s ) ->
                        Pitch.unwrapDegree o
                            == Pitch.unwrapDegree s
                            && Pitch.unwrapAccidental o
                            == Pitch.unwrapAccidental s

                    _ ->
                        False
            )


viewPitch : Pitch -> Html msg
viewPitch pitch =
    case Pitch.unwrapAccidental pitch of
        Nothing ->
            Degree.textOctave (Pitch.unwrapDegree pitch)

        Just accidental ->
            span []
                [ Degree.textOctave (Pitch.unwrapDegree pitch)
                , text (" " ++ Accidental.toString accidental)
                ]



-- CLASSIFICATION


{-| I think "Classification" (or maybe "OktoichosClassification") will work as a
term, at least for internal conceptual modeling. How this is exposed towards
users is a different question.

I'm wondering if it would be better to have just a flat list of eight rather than
a product type.

-}
basesForClassification : Classification -> List Pitch
basesForClassification (Classification division ordinal) =
    case ( division, ordinal ) of
        ( Authentic, ModeOne ) ->
            [ Pitch.natural Ke, Pitch.natural Pa ]

        ( Authentic, ModeTwo ) ->
            [ Pitch.natural Di, Pitch.natural Pa ]

        ( Authentic, ModeThree ) ->
            [ Pitch.natural Ga ]

        ( Authentic, ModeFour ) ->
            [ Pitch.natural Bou, Pitch.natural Di ]

        ( Plagal, ModeOne ) ->
            [ Pitch.natural Ke, Pitch.natural Pa ]

        ( Plagal, ModeTwo ) ->
            [ Pitch.natural Di, Pitch.natural Bou ]

        ( Plagal, ModeThree ) ->
            [ Pitch.natural Ga
            , Pitch.natural Zo_
            , Pitch.inflected Enharmonic Flat4 Zo_
                |> Result.withDefault (Pitch.natural Zo_)
            ]

        ( Plagal, ModeFour ) ->
            [ Pitch.natural Ni, Pitch.natural Ga ]


allClassifications : List Classification
allClassifications =
    [ Classification Authentic ModeOne
    , Classification Authentic ModeTwo
    , Classification Authentic ModeThree
    , Classification Authentic ModeFour
    , Classification Plagal ModeOne
    , Classification Plagal ModeTwo
    , Classification Plagal ModeThree
    , Classification Plagal ModeFour
    ]



-- List.Extra.cartesianProduct divisions ordinals


type Classification
    = Classification Division Ordinal


type Division
    = Authentic
    | Plagal


type Ordinal
    = ModeOne
    | ModeTwo
    | ModeThree
    | ModeFour


toString : Classification -> String
toString (Classification division ordinal) =
    case ( division, ordinal ) of
        ( Authentic, ModeOne ) ->
            "Authentic Mode One"

        ( Authentic, ModeTwo ) ->
            "Authentic Mode Two"

        ( Authentic, ModeThree ) ->
            "Authentic Mode Three"

        ( Authentic, ModeFour ) ->
            "Authentic Mode Four"

        ( Plagal, ModeOne ) ->
            "Plagal Mode One"

        ( Plagal, ModeTwo ) ->
            "Plagal Mode Two"

        ( Plagal, ModeThree ) ->
            "Grave Mode"

        ( Plagal, ModeFour ) ->
            "Plagal Mode Four"



{-
   A Mode can be generally built from a classification plus a base.

   A base is a pitch (which, in all but one case, will be natural, but
   there does exist a grave variant on Ζω-flat, which prevents a simple
   definition on degree).



   But, not all pitches are actual bases.

   Bases:

   - Authentic 1: Κε, Πα
   - Authentic 2: Δι, Πα -- Βου
   - Authentic 3: Γα
   - Authentic 4: Βου, Δι -- Πα, maybe?
   - Plagal 1: Κε, Πα
   - Plagal 2: Δι, Βου
   - Plagal 3: Γα, Ζω, Ζω-flat4
   - Plagal 4: Νη, Γα

-}
