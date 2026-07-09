module ModeBuilder exposing (Model, Msg, Result(..), currentResult, init, update, view)

{-| Decision-tree component for selecting a Byzantine mode or scale.

Three strategies are available:

  - **By Scale** — choose one of the four canonical scales directly.
  - **By Classification** — choose an Oktoichos classification, then a base pitch.
  - **By Signature** — choose an Ichos, then an indicator, then a specific modal signature.

The current terminal selection is queried via `currentResult`.

-}

import Byzantine.Accidental as Accidental
import Byzantine.ByzHtml.ModalSignature as ByzHtml
import Byzantine.Degree as Degree exposing (Degree)
import Byzantine.Fthora exposing (DiatonicDegree(..), Fthora(..))
import Byzantine.Mode.Classification as Classification exposing (Classification)
import Byzantine.Mode.Signature as Signature exposing (Ichos(..), Indicator(..), Signature)
import Byzantine.Pitch as Pitch exposing (Pitch)
import Byzantine.Scale as Scale exposing (Scale)
import Components.AnimatedHeight as AnimatedHeight
import Components.RadioFieldset as RadioFieldset
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Extra
import Html.Keyed
import Maybe.Extra
import Styles



-- STRATEGY


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



-- STEP
-- Encodes the user's current position in the decision tree.
-- Non-terminal steps carry the upstream context needed to render their options.


type Step
    = SelectingStrategy
    | SelectingScale
    | SelectingClassification
    | SelectingBase Classification
    | SelectingIchos
    | SelectingIndicator Ichos
    | SelectingSignature Ichos Indicator



-- DIRECTION


type Direction
    = Forward
    | Backward



-- RESULT


type Result
    = ByScale Scale
    | ByClassification Classification Pitch
    | BySignature Signature



-- SELECTIONS
-- All selections are persisted across back navigation so each panel
-- re-renders with the previously chosen radio already checked.


type alias Selections =
    { strategy : Maybe Strategy
    , scale : Maybe Scale
    , classification : Maybe Classification
    , base : Maybe Pitch
    , ichos : Maybe Ichos
    , indicator : Maybe Indicator
    , signature : Maybe Signature
    }


emptySelections : Selections
emptySelections =
    { strategy = Nothing
    , scale = Nothing
    , classification = Nothing
    , base = Nothing
    , ichos = Nothing
    , indicator = Nothing
    , signature = Nothing
    }



-- MODEL


type alias Model =
    { step : Step
    , history : List Step -- stack, head = most recently visited node; supports GoBack
    , direction : Maybe Direction -- Nothing on first render; drives CSS entrance animation
    , selections : Selections
    }


init : Model
init =
    { step = SelectingStrategy
    , history = []
    , direction = Nothing
    , selections = emptySelections
    }



-- RESULT QUERY


{-| Returns the terminal selection if the user has reached and completed a
leaf node; Nothing if they are still navigating.
-}
currentResult : Model -> Maybe Result
currentResult { step, selections } =
    case step of
        SelectingScale ->
            Maybe.map ByScale selections.scale

        SelectingBase _ ->
            Maybe.map2 ByClassification selections.classification selections.base

        SelectingSignature _ _ ->
            Maybe.map BySignature selections.signature

        _ ->
            Nothing



-- MSG


type Msg
    = SelectStrategy (Maybe Strategy)
    | SelectScale (Maybe Scale)
    | SelectClassification (Maybe Classification)
    | SelectBase (Maybe Pitch)
    | SelectIchos (Maybe Ichos)
    | SelectIndicator (Maybe Indicator)
    | SelectSignature (Maybe Signature)
    | GoBack



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        sel =
            model.selections
    in
    case msg of
        SelectStrategy maybeStrategy ->
            case maybeStrategy of
                Nothing ->
                    ( model, Cmd.none )

                Just strategy ->
                    let
                        nextStep =
                            case strategy of
                                SelectByScale ->
                                    SelectingScale

                                SelectByClassification ->
                                    SelectingClassification

                                SelectBySignature ->
                                    SelectingIchos
                    in
                    ( { model
                        | step = nextStep
                        , history = SelectingStrategy :: model.history
                        , direction = Just Forward
                        , selections = { emptySelections | strategy = Just strategy }
                      }
                    , Cmd.none
                    )

        SelectScale maybeScale ->
            ( { model | selections = { sel | scale = maybeScale } }, Cmd.none )

        SelectClassification maybeClassification ->
            case maybeClassification of
                Nothing ->
                    ( model, Cmd.none )

                Just classification ->
                    ( { model
                        | step = SelectingBase classification
                        , history = model.step :: model.history
                        , direction = Just Forward
                        , selections = { sel | classification = Just classification, base = Nothing }
                      }
                    , Cmd.none
                    )

        SelectBase maybeBase ->
            ( { model | selections = { sel | base = maybeBase } }, Cmd.none )

        SelectIchos maybeIchos ->
            case maybeIchos of
                Nothing ->
                    ( model, Cmd.none )

                Just ichos ->
                    ( { model
                        | step = SelectingIndicator ichos
                        , history = model.step :: model.history
                        , direction = Just Forward
                        , selections =
                            { sel
                                | ichos = Just ichos
                                , indicator = Nothing
                                , signature = Nothing
                            }
                      }
                    , Cmd.none
                    )

        SelectIndicator maybeIndicator ->
            case ( maybeIndicator, model.step ) of
                ( Just indicator, SelectingIndicator ichos ) ->
                    ( { model
                        | step = SelectingSignature ichos indicator
                        , history = model.step :: model.history
                        , direction = Just Forward
                        , selections = { sel | indicator = Just indicator, signature = Nothing }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SelectSignature maybeSignature ->
            ( { model | selections = { sel | signature = maybeSignature } }, Cmd.none )

        GoBack ->
            case model.history of
                [] ->
                    ( model, Cmd.none )

                previousStep :: rest ->
                    ( { model
                        | step = previousStep
                        , history = rest
                        , direction = Just Backward
                      }
                    , Cmd.none
                    )



-- VIEW


view : Model -> Html Msg
view model =
    div [ Styles.flexCol, class "gap-1" ]
        [ viewNav model
        , AnimatedHeight.view
            (Html.Keyed.node "div"
                []
                [ ( stepKey model.step
                  , div [ class (animationClass model.direction) ]
                        [ viewStep model ]
                  )
                ]
            )
        ]


viewNav : Model -> Html Msg
viewNav model =
    if List.isEmpty model.history then
        Html.Extra.nothing

    else
        div [ Styles.flexRow, class "items-center gap-2 px-2 pb-1" ]
            [ button [ Styles.buttonClass, onClick GoBack ]
                [ text "← Back" ]
            , viewBreadcrumb model
            ]


viewBreadcrumb : Model -> Html Msg
viewBreadcrumb model =
    let
        parts =
            model.history
                |> List.reverse
                |> List.filterMap (breadcrumbLabel model.selections)
    in
    div [ class "text-sm text-gray-500 truncate" ]
        (List.intersperse " / " parts |> List.map text)


{-| Returns the label for the selection made _at_ a given history step,
used to build the breadcrumb trail. Terminal steps and the Base step
(whose classification is already shown by its parent) return Nothing.
-}
breadcrumbLabel : Selections -> Step -> Maybe String
breadcrumbLabel sel step =
    case step of
        SelectingStrategy ->
            Maybe.map strategyToString sel.strategy

        SelectingClassification ->
            Maybe.map Classification.toString sel.classification

        SelectingIchos ->
            Maybe.map ichosToString sel.ichos

        SelectingIndicator _ ->
            Maybe.map indicatorToString sel.indicator

        SelectingScale ->
            Nothing

        SelectingBase _ ->
            Nothing

        SelectingSignature _ _ ->
            Nothing


stepKey : Step -> String
stepKey step =
    case step of
        SelectingStrategy ->
            "strategy"

        SelectingScale ->
            "scale"

        SelectingClassification ->
            "classification"

        SelectingBase _ ->
            "base"

        SelectingIchos ->
            "ichos"

        SelectingIndicator _ ->
            "indicator"

        SelectingSignature _ _ ->
            "signature"


animationClass : Maybe Direction -> String
animationClass dir =
    case dir of
        Nothing ->
            ""

        Just Forward ->
            "slide-in-forward"

        Just Backward ->
            "slide-in-backward"


viewStep : Model -> Html Msg
viewStep model =
    case model.step of
        SelectingStrategy ->
            RadioFieldset.view strategyRadioConfig model.selections.strategy

        SelectingScale ->
            RadioFieldset.view scaleRadioConfig model.selections.scale

        SelectingClassification ->
            RadioFieldset.view classificationRadioConfig model.selections.classification

        SelectingBase classification ->
            RadioFieldset.view
                (baseRadioConfig (Classification.basesFor classification))
                model.selections.base

        SelectingIchos ->
            RadioFieldset.view ichosRadioConfig model.selections.ichos

        SelectingIndicator ichos ->
            RadioFieldset.view (indicatorRadioConfig ichos) model.selections.indicator

        SelectingSignature ichos indicator ->
            RadioFieldset.view (signatureRadioConfig ichos indicator) model.selections.signature



-- RADIO CONFIGS


strategyRadioConfig : RadioFieldset.Config (Maybe Strategy) Msg
strategyRadioConfig =
    RadioFieldset.baseConfig
        { itemToString = Maybe.Extra.unwrap "" strategyToString >> (++) "mode-builder-strategy-"
        , legendText = "Selection Strategy"
        , onSelect = SelectStrategy
        , options = List.map Just [ SelectByScale, SelectByClassification, SelectBySignature ]
        }
        |> RadioFieldset.withCustomViewItem
            (Html.Extra.viewMaybe (text << strategyToString))


scaleRadioConfig : RadioFieldset.Config (Maybe Scale) Msg
scaleRadioConfig =
    RadioFieldset.baseConfig
        { itemToString = Maybe.Extra.unwrap "" Scale.name >> (++) "mode-builder-scale-"
        , legendText = "Scale"
        , onSelect = SelectScale
        , options = List.map Just Scale.all
        }
        |> RadioFieldset.withCustomViewItem
            (Html.Extra.viewMaybe (text << Scale.name))


classificationRadioConfig : RadioFieldset.Config (Maybe Classification) Msg
classificationRadioConfig =
    RadioFieldset.baseConfig
        { itemToString = Maybe.Extra.unwrap "" Classification.toString >> (++) "mode-builder-class-"
        , legendText = "Classification"
        , onSelect = SelectClassification
        , options = List.map Just Classification.all
        }
        |> RadioFieldset.withCustomViewItem
            (Html.Extra.viewMaybe (text << Classification.toString))
        |> RadioFieldset.withLayout RadioFieldset.TwoColumnGrid


baseRadioConfig : List Pitch -> RadioFieldset.Config (Maybe Pitch) Msg
baseRadioConfig bases =
    RadioFieldset.baseConfig
        { itemToString = Maybe.Extra.unwrap "" Pitch.toString >> (++) "mode-builder-base-"
        , legendText = "Base"
        , onSelect = SelectBase
        , options = List.map Just bases
        }
        |> RadioFieldset.withCustomViewItem
            (Html.Extra.viewMaybe viewPitch)


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
                        "mode-builder-ichos-none"
        , legendText = "Ichos"
        , onSelect = SelectIchos
        , options = [ Just Ichos, Just IchosPlagal ]
        }
        |> RadioFieldset.withCustomViewItem
            (Html.Extra.viewMaybe
                (div [ class "text-2xl mt-1" ] << ByzHtml.viewDivision)
            )


indicatorRadioConfig : Ichos -> RadioFieldset.Config (Maybe Indicator) Msg
indicatorRadioConfig ichos =
    RadioFieldset.baseConfig
        { itemToString = Maybe.Extra.unwrap "" indicatorToString >> (++) "mode-builder-indicator-"
        , legendText = "Indicator"
        , onSelect = SelectIndicator
        , options = List.map Just (Signature.indicatorsFor ichos)
        }
        |> RadioFieldset.withCustomViewItem
            (Html.Extra.viewMaybe
                (\ind -> div [ class "text-2xl mt-1" ] [ ByzHtml.viewIndicator ind ])
            )


signatureRadioConfig : Ichos -> Indicator -> RadioFieldset.Config (Maybe Signature) Msg
signatureRadioConfig ichos indicator =
    RadioFieldset.baseConfig
        { itemToString = Maybe.Extra.unwrap "" signatureTag >> (++) "mode-builder-sig-"
        , legendText = "Signature"
        , onSelect = SelectSignature
        , options = List.map Just (Signature.signaturesFor ichos indicator)
        }
        |> RadioFieldset.withCustomViewItem
            (Html.Extra.viewMaybe
                (\sig -> div [ class "text-2xl mt-1" ] [ ByzHtml.view sig ])
            )



-- HELPERS: DISPLAY


ichosToString : Ichos -> String
ichosToString ichos =
    case ichos of
        Ichos ->
            "Ichos"

        IchosPlagal ->
            "Ichos Plagal"


indicatorToString : Indicator -> String
indicatorToString indicator =
    case indicator of
        First ->
            "First"

        Second ->
            "Second"

        Third ->
            "Third"

        ThirdNaNa ->
            "Third Nana"

        Fourth ->
            "Fourth"

        Legetos ->
            "Legetos"

        PlagalFirst ->
            "Plagal First"

        PlagalSecond ->
            "Plagal Second"

        Varys ->
            "Varys"

        VarysZo ->
            "Varys Zo"

        PlagalFourth ->
            "Plagal Fourth"

        ClassificationIndicator c ->
            Classification.toString c


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
            span []
                [ Degree.text (Pitch.unwrapDegree pitch)
                , text (" " ++ Accidental.toString accidental)
                ]



-- HELPERS: SIGNATURE ID
-- Derives a unique string key per signature for use as a radio element ID.
-- The combination of base degree + fthora type is unique within any
-- (Ichos, Indicator)-filtered subset of Signature.all.


signatureTag : Signature -> String
signatureTag sig =
    let
        e =
            Signature.elements sig

        degStr =
            Maybe.Extra.unwrap "x" Degree.toString e.baseDegree

        fthStr =
            Maybe.Extra.unwrap "x" fthoraTag e.fthora
    in
    degStr ++ "-" ++ fthStr


fthoraTag : Fthora -> String
fthoraTag fthora =
    case fthora of
        DiatonicFthora d ->
            "d-" ++ diatonicDegTag d

        EnharmonicFthora _ ->
            "e"

        SoftChromaticFthora _ ->
            "sc"

        HardChromaticFthora _ ->
            "hc"


diatonicDegTag : DiatonicDegree -> String
diatonicDegTag d =
    case d of
        D_Ni ->
            "ni"

        D_Pa ->
            "pa"

        D_Bou ->
            "bou"

        D_Ga ->
            "ga"

        D_Di ->
            "di"

        D_Ke ->
            "ke"

        D_Zo_ ->
            "zo"

        D_Ni_ ->
            "ni-upper"
