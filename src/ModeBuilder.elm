module ModeBuilder exposing (Model, Msg, init, update, view)

{-| Experimental modeling for constructing a mode.
-}

-- import Byzantine.Mode.Signature as Signature

import Byzantine.Accidental as Accidental
import Byzantine.ByzHtml.ModalSignature as ByzHtml
import Byzantine.Degree as Degree exposing (Degree)
import Byzantine.Fthora exposing (Fthora)
import Byzantine.Mode.Classification as Classification exposing (Classification)
import Byzantine.Mode.Signature exposing (Ichos(..), Indicator)
import Byzantine.Pitch as Pitch exposing (Pitch)
import Byzantine.Scale as Scale exposing (Scale)
import Components.RadioFieldset as RadioFieldset
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Extra
import Maybe.Extra
import Styles



-- MODEL


type alias Model =
    { base : Maybe Pitch
    , classification : Maybe Classification
    , scale : Maybe Scale
    , signatureBuilder : Maybe SignatureBuilder
    , strategy : Strategy
    }


init : Model
init =
    { base = Nothing
    , classification = Nothing
    , scale = Nothing
    , signatureBuilder = Nothing
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


type alias SignatureBuilder =
    { ichos : Maybe Ichos
    , indicator : Maybe Indicator
    , baseDegree : Maybe Degree
    , baseFthora : Maybe Fthora
    }


emptySignatureBuilder : SignatureBuilder
emptySignatureBuilder =
    { ichos = Nothing
    , indicator = Nothing
    , baseDegree = Nothing
    , baseFthora = Nothing
    }


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
    | SelectIchos (Maybe Ichos)



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

        SelectIchos ichos ->
            let
                builder =
                    model.signatureBuilder
                        |> Maybe.withDefault emptySignatureBuilder
            in
            ( { model
                | signatureBuilder =
                    Just { builder | ichos = ichos }
              }
            , Cmd.none
            )



-- updateSignatureBuilder :
-- VIEW


view : Model -> Html Msg
view model =
    div [ Styles.flexRow, class "p-4 gap-2" ]
        [ RadioFieldset.view strategyRadioConfig model.strategy
        , case model.strategy of
            SelectByScale ->
                RadioFieldset.view scaleRadioConfig model.scale

            SelectByClassification ->
                RadioFieldset.view classificationRadioConfig model.classification

            SelectBySignature ->
                RadioFieldset.view ichosRadioConfig
                    (model.signatureBuilder
                        |> Maybe.andThen .ichos
                    )

        -- (RadioFieldset.view classificationRadioConfig model.classification)
        , Html.Extra.viewMaybe
            (\classification ->
                RadioFieldset.view
                    (baseRadioConfig (Classification.basesFor classification))
                    model.base
            )
            model.classification

        -- , div [ Styles.flexCol, class "gap-2" ]
        --     (List.map ByzHtml.view Signature.all)
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
        |> RadioFieldset.withLayout RadioFieldset.TwoColumnGrid


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


ichosRadioConfig : RadioFieldset.Config (Maybe Ichos) Msg
ichosRadioConfig =
    RadioFieldset.baseConfig
        { itemToString =
            \ichos ->
                case ichos of
                    Just Ichos ->
                        "mode-builder-ichos"

                    Just IchosPlagal ->
                        "mode-builder-ichos-plagal"

                    Nothing ->
                        "mode-builder-"
        , legendText = "Ichos"
        , onSelect = SelectIchos
        , options = [ Just Ichos, Just IchosPlagal ]
        }
        |> RadioFieldset.withCustomViewItem
            (Html.Extra.viewMaybe
                (div [ class "text-xl mt-2" ] << ByzHtml.viewDivision)
            )
