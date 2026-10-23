module ModeBuilder exposing (Model, Msg, Selection(..), currentSelection, init, update, view)

{-| Decision-tree component for selecting a Byzantine mode or scale.

Three strategies are available:

  - **By Scale** — choose one of the four canonical scales directly.
  - **By Classification** — choose an Oktoichos classification, then
    pick from the full modal signatures belonging to that classification.
  - **By Signature** — choose an Ichos, then an indicator, then a specific modal signature.

Both signature-based strategies resolve to a `Mode`. When the chosen signature is
shared by several modes that differ by genre (e.g., Lower Mode One, Eirmologic
vs. Sticheraric), a genre selector appears beneath the signatures and must be
answered before a selection is available.

The current terminal selection is queried via `currentSelection`.

-}

import Byzantine.ByzHtml.ModalSignature as ByzHtml
import Byzantine.Degree as Degree
import Byzantine.Fthora
    exposing
        ( DiatonicDegree(..)
        , EnharmonicDegree(..)
        , Fthora(..)
        , HardChromaticDegree(..)
        , SoftChromaticDegree(..)
        )
import Byzantine.Mode as Mode exposing (Mode)
import Byzantine.Mode.Classification as Classification exposing (Classification)
import Byzantine.Mode.Genre as Genre
import Byzantine.Mode.Signature as Signature exposing (Ichos(..), Indicator(..), Signature)
import Byzantine.Scale as Scale exposing (Scale)
import Components.AnimatedHeight as AnimatedHeight
import Components.RadioFieldset as RadioFieldset
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Extra
import Html.Keyed
import List.Extra
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
    | SelectingSignatureForClassification Classification
    | SelectingIchos
    | SelectingIndicator Ichos
    | SelectingSignature Ichos Indicator



-- DIRECTION


type Direction
    = Forward
    | Backward



-- SELECTION


type Selection
    = ByScale Scale
    | ByMode Mode



-- SELECTIONS
-- All selections are persisted across back navigation so each panel
-- re-renders with the previously chosen radio already checked.


type alias Selections =
    { strategy : Maybe Strategy
    , scale : Maybe Scale
    , classification : Maybe Classification
    , ichos : Maybe Ichos
    , indicator : Maybe Indicator
    , signature : Maybe Signature
    , mode : Maybe Mode -- only used to disambiguate a signature shared by several modes
    }


emptySelections : Selections
emptySelections =
    { strategy = Nothing
    , scale = Nothing
    , classification = Nothing
    , ichos = Nothing
    , indicator = Nothing
    , signature = Nothing
    , mode = Nothing
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



-- SELECTION QUERY


{-| Returns the terminal selection if the user has reached and completed a
leaf node; Nothing if they are still navigating.
-}
currentSelection : Model -> Maybe Selection
currentSelection { step, selections } =
    case step of
        SelectingScale ->
            Maybe.map ByScale selections.scale

        SelectingSignature _ _ ->
            resolveSelectedMode selections

        SelectingSignatureForClassification _ ->
            resolveSelectedMode selections

        _ ->
            Nothing



-- MSG


type Msg
    = SelectStrategy (Maybe Strategy)
    | SelectScale (Maybe Scale)
    | SelectClassification (Maybe Classification)
    | SelectIchos (Maybe Ichos)
    | SelectIndicator (Maybe Indicator)
    | SelectSignature (Maybe Signature)
    | SelectMode (Maybe Mode)
    | GoBack



-- UPDATE


{-| Handle a `Msg`, returning the new model, any command, and the current
terminal `Selection` (if the user has completed one).
-}
update : Msg -> Model -> ( Model, Cmd Msg, Maybe Selection )
update msg model =
    let
        ( newModel, cmd ) =
            updateModel msg model
    in
    ( newModel, cmd, currentSelection newModel )


updateModel : Msg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
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
                    advanceTo (stepForStrategy strategy)
                        { emptySelections | strategy = Just strategy }
                        model

        SelectScale maybeScale ->
            ( { model | selections = { sel | scale = maybeScale } }, Cmd.none )

        SelectClassification maybeClassification ->
            case maybeClassification of
                Nothing ->
                    ( model, Cmd.none )

                Just classification ->
                    advanceTo (SelectingSignatureForClassification classification)
                        { sel | classification = Just classification, signature = Nothing, mode = Nothing }
                        model

        SelectIchos maybeIchos ->
            case maybeIchos of
                Nothing ->
                    ( model, Cmd.none )

                Just ichos ->
                    advanceTo (SelectingIndicator ichos)
                        { sel | ichos = Just ichos, indicator = Nothing, signature = Nothing, mode = Nothing }
                        model

        SelectIndicator maybeIndicator ->
            case ( maybeIndicator, model.step ) of
                ( Just indicator, SelectingIndicator ichos ) ->
                    advanceTo (SelectingSignature ichos indicator)
                        { sel | indicator = Just indicator, signature = Nothing, mode = Nothing }
                        model

                _ ->
                    ( model, Cmd.none )

        SelectSignature maybeSignature ->
            ( { model | selections = { sel | signature = maybeSignature, mode = Nothing } }, Cmd.none )

        SelectMode maybeMode ->
            ( { model | selections = { sel | mode = maybeMode } }, Cmd.none )

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


{-| The first step of the wizard branch that each strategy leads into once the
user has chosen how they want to build their mode.
-}
stepForStrategy : Strategy -> Step
stepForStrategy strategy =
    case strategy of
        SelectByScale ->
            SelectingScale

        SelectByClassification ->
            SelectingClassification

        SelectBySignature ->
            SelectingIchos


{-| Move the wizard forward to `nextStep`, applying the given selections.

The current step is pushed onto the history stack so `GoBack` can return to it,
and the direction is set to `Forward` to drive the entrance animation.

-}
advanceTo : Step -> Selections -> Model -> ( Model, Cmd Msg )
advanceTo nextStep selections model =
    ( { model
        | step = nextStep
        , history = model.step :: model.history
        , direction = Just Forward
        , selections = selections
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Html Msg
view model =
    div [ Styles.flexCol, class "gap-1 max-w-lg mx-auto" ]
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
used to build the breadcrumb trail. Terminal steps return Nothing.
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

        SelectingSignature _ _ ->
            Nothing

        SelectingSignatureForClassification _ ->
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

        SelectingSignatureForClassification _ ->
            "signature-for-classification"

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

        SelectingSignatureForClassification classification ->
            div []
                [ RadioFieldset.view
                    (classifiedSignatureRadioConfig classification)
                    model.selections.signature
                , viewGenreSelector model.selections
                ]

        SelectingIchos ->
            RadioFieldset.view ichosRadioConfig model.selections.ichos

        SelectingIndicator ichos ->
            RadioFieldset.view (indicatorRadioConfig ichos) model.selections.indicator

        SelectingSignature ichos indicator ->
            div []
                [ RadioFieldset.view
                    (signatureRadioConfig ichos indicator)
                    model.selections.signature
                , viewGenreSelector model.selections
                ]



-- RADIO CONFIGS


strategyRadioConfig : RadioFieldset.Config (Maybe Strategy) Msg
strategyRadioConfig =
    RadioFieldset.baseConfig
        { itemToString = Maybe.Extra.unwrap "" strategyToString >> (++) "mode-builder-strategy-"
        , legendText = "Selection Strategy"
        , onSelect = SelectStrategy
        , options =
            List.map Just
                [ SelectByScale
                , SelectByClassification
                , SelectBySignature
                ]
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


{-| The classification options rendered as a Byzantine glyph followed by the
plain-text name in parentheses.
-}
classificationRadioConfig : RadioFieldset.Config (Maybe Classification) Msg
classificationRadioConfig =
    RadioFieldset.baseConfig
        { itemToString = Maybe.Extra.unwrap "" Classification.toString >> (++) "mode-builder-class-"
        , legendText = "Classification"
        , onSelect = SelectClassification
        , options = List.map Just Classification.all
        }
        |> RadioFieldset.withCustomViewItem
            (Html.Extra.viewMaybe
                (\classification ->
                    div [ Styles.flexRow, class "items-center gap-2 mt-1" ]
                        [ span [ class "text-2xl" ] [ ByzHtml.viewClassification classification ]
                        , span [] [ text ("(" ++ Classification.toString classification ++ ")") ]
                        ]
                )
            )
        |> RadioFieldset.withLayout RadioFieldset.TwoColumnGrid


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
        |> RadioFieldset.withLayout RadioFieldset.TwoColumnGrid


indicatorRadioConfig : Ichos -> RadioFieldset.Config (Maybe Indicator) Msg
indicatorRadioConfig ichos =
    RadioFieldset.baseConfig
        { itemToString = Maybe.Extra.unwrap "" indicatorToString >> (++) "mode-builder-indicator-"
        , legendText = "Indicator"
        , onSelect = SelectIndicator
        , options = List.map Just (indicatorsWithSignatures ichos)
        }
        |> RadioFieldset.withCustomViewItem
            (Html.Extra.viewMaybe
                (\ind ->
                    div [ class "text-2xl mt-1" ]
                        (ByzHtml.viewDivision ichos
                            ++ [ ByzHtml.viewIndicator ind ]
                        )
                )
            )
        |> RadioFieldset.withLayout RadioFieldset.TwoColumnGrid


{-| The indicators for an Ichos that lead to at least one signature, so the user
is never sent to a step with nothing to select.
-}
indicatorsWithSignatures : Ichos -> List Indicator
indicatorsWithSignatures ichos =
    Signature.indicatorsFor ichos
        |> List.filter
            (\indicator -> not (List.isEmpty (Signature.signaturesFor ichos indicator)))


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


classifiedSignatureRadioConfig : Classification -> RadioFieldset.Config (Maybe Signature) Msg
classifiedSignatureRadioConfig classification =
    RadioFieldset.baseConfig
        { itemToString = Maybe.Extra.unwrap "" signatureTag >> (++) "mode-builder-sig-"
        , legendText = "Signature"
        , onSelect = SelectSignature
        , options = List.map Just (Signature.signaturesForClassification classification)
        }
        |> RadioFieldset.withCustomViewItem
            (Html.Extra.viewMaybe
                (\sig -> div [ class "text-2xl mt-1" ] [ ByzHtml.view sig ])
            )
        |> RadioFieldset.withLayout RadioFieldset.TwoColumnGrid


genreRadioConfig : List Mode -> RadioFieldset.Config (Maybe Mode) Msg
genreRadioConfig modes =
    RadioFieldset.baseConfig
        { itemToString = Maybe.Extra.unwrap "" modeTag >> (++) "mode-builder-genre-"
        , legendText = "Genre"
        , onSelect = SelectMode
        , options = List.map Just modes
        }
        |> RadioFieldset.withCustomViewItem
            (Html.Extra.viewMaybe (text << genreLabel))
        |> RadioFieldset.withLayout RadioFieldset.TwoColumnGrid


{-| The genre selector, shown only once a signature is chosen that is shared by
more than one mode. Otherwise the signature already determines the mode.
-}
viewGenreSelector : Selections -> Html Msg
viewGenreSelector selections =
    case Maybe.map modesForSignature selections.signature of
        Just ((_ :: _ :: _) as modes) ->
            RadioFieldset.view (genreRadioConfig modes) selections.mode

        _ ->
            Html.Extra.nothing



-- HELPERS: MODES


{-| All modes that use the given signature.
-}
modesForSignature : Signature -> List Mode
modesForSignature signature =
    List.filter
        (\mode -> List.member signature (Mode.data mode).signatures)
        Mode.all


{-| The mode the current selections lead to, if the user has chosen a signature
and, where needed, a genre.
-}
resolveSelectedMode : Selections -> Maybe Selection
resolveSelectedMode selections =
    selections.signature
        |> Maybe.andThen (resolveMode selections.mode)
        |> Maybe.map ByMode


{-| The mode a signature leads to: the only mode that uses it, or, when several
do, the user's genre choice (if it is one of them).
-}
resolveMode : Maybe Mode -> Signature -> Maybe Mode
resolveMode chosen signature =
    case modesForSignature signature of
        [ only ] ->
            Just only

        modes ->
            chosen
                |> Maybe.andThen (\mode -> List.Extra.find ((==) mode) modes)


{-| Label for a mode in the genre selector: its genre(s), falling back to the
mode's full name for modes that don't declare any.
-}
genreLabel : Mode -> String
genreLabel mode =
    case (Mode.data mode).genres of
        [] ->
            Mode.toString mode

        genres ->
            genres |> List.map Genre.toString |> String.join ", "


{-| Element ID fragment, unique per mode.
-}
modeTag : Mode -> String
modeTag mode =
    List.Extra.elemIndex mode Mode.all
        |> Maybe.withDefault -1
        |> String.fromInt



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



-- HELPERS: SIGNATURE ID
-- Derives a unique string key per signature for use as a radio element ID.
-- The combination of indicator + base degree + fthora is unique within any
-- Classification-filtered subset of Signature.all (and therefore also within any
-- (Ichos, Indicator)-filtered subset).


signatureTag : Signature -> String
signatureTag sig =
    let
        e =
            Signature.elements sig

        indicatorStr =
            indicatorToString e.indicator
                |> String.toLower
                |> String.replace " " "-"

        degStr =
            Maybe.Extra.unwrap "x" Degree.toString e.baseDegree

        fthStr =
            Maybe.Extra.unwrap "x" fthoraTag e.fthora
    in
    indicatorStr ++ "-" ++ degStr ++ "-" ++ fthStr


fthoraTag : Fthora -> String
fthoraTag fthora =
    case fthora of
        DiatonicFthora d ->
            "d-" ++ diatonicDegTag d

        EnharmonicFthora d ->
            "e-" ++ enharmonicDegTag d

        SoftChromaticFthora d ->
            "sc-" ++ softChromaticDegTag d

        HardChromaticFthora d ->
            "hc-" ++ hardChromaticDegTag d


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


enharmonicDegTag : EnharmonicDegree -> String
enharmonicDegTag d =
    case d of
        E_Ga ->
            "ga"

        E_Ke ->
            "ke"

        E_Zo_ ->
            "zo"


softChromaticDegTag : SoftChromaticDegree -> String
softChromaticDegTag d =
    case d of
        SC_Di ->
            "di"

        SC_Ke ->
            "ke"


hardChromaticDegTag : HardChromaticDegree -> String
hardChromaticDegTag d =
    case d of
        HC_Pa ->
            "pa"

        HC_Di ->
            "di"
