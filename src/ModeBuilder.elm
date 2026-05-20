module ModeBuilder exposing (Model, Msg, init, update, view)

{-| Experimental modeling for constructing a mode.
-}

import Byzantine.Accidental as Accidental
import Byzantine.ByzHtml.ModalSignature as ByzHtml
import Byzantine.Degree as Degree
import Byzantine.ModalSignature as ModalSignature
import Byzantine.Mode.Classification as Classification exposing (Classification)
import Byzantine.Pitch as Pitch exposing (Pitch)
import Byzantine.Scale as Scale exposing (Scale)
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
                    (baseRadioConfig (Classification.basesFor classification))
                    model.base
            )
            model.classification
        , div [ Styles.flexCol, class "gap-2" ]
            (List.map ByzHtml.view ModalSignature.all)
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
        { itemToString = Maybe.Extra.unwrap "" Classification.toString >> (++) "mode-builder-"
        , legendText = "Classification"
        , onSelect = SelectClassification
        , options = List.map Just Classification.all
        }
        |> RadioFieldset.withCustomViewItem
            (Html.Extra.viewMaybe text << Maybe.map Classification.toString)


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


viewPitch : Pitch -> Html msg
viewPitch pitch =
    case Pitch.unwrapAccidental pitch of
        Nothing ->
            Degree.text (Pitch.unwrapDegree pitch)

        Just Accidental.Flat4 ->
            span []
                [ Degree.text (Pitch.unwrapDegree pitch)
                , text "-flat"
                ]

        Just accidental ->
            -- this should never happen
            span []
                [ Degree.text (Pitch.unwrapDegree pitch)
                , text (" " ++ Accidental.toString accidental)
                ]
