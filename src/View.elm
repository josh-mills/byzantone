module View exposing (view)

import Byzantine.ByzHtml.Martyria as ByzHtmlMartyria
import Byzantine.Degree as Degree exposing (Degree(..))
import Byzantine.Frequency as Frequency exposing (Frequency(..), PitchStandard(..))
import Byzantine.Martyria as Martyria
import Byzantine.Mode as Mode exposing (Mode)
import Byzantine.Pitch as Pitch exposing (Pitch)
import Byzantine.Scale exposing (Scale(..))
import Copy
import Html exposing (Html, button, datalist, div, h1, h2, main_, p, span, text)
import Html.Attributes as Attr exposing (class, classList, id, type_)
import Html.Attributes.Extra as Attr
import Html.Events exposing (onClick, onInput)
import Html.Extra exposing (viewIf)
import Html.Lazy exposing (lazy, lazy2, lazy3, lazy4, lazy5)
import Http
import Icons
import Json.Decode exposing (Decoder)
import Maybe.Extra as Maybe
import Model exposing (Modal(..), Model)
import Model.AudioSettings as AudioSettings exposing (AudioSettings)
import Model.Changelog exposing (Changelog)
import Model.LayoutData as LayoutData exposing (Layout(..), LayoutData, LayoutSelection(..), layoutFor)
import Model.ModeSettings exposing (ModeSettings)
import Model.PitchState as PitchState
import RadioFieldset
import RemoteData exposing (RemoteData)
import Styles
import Svg.Attributes
import Update exposing (Msg(..))
import View.Changelog
import View.Controls
import View.ModeData
import View.PitchSpace as PitchSpace



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "h-screen" ]
        [ Html.Extra.viewIfLazy (model.audioSettings.audioMode == AudioSettings.Play)
            (\_ ->
                lazy4 chantEngineNode
                    model.audioSettings
                    model.modeSettings.scale
                    (PitchState.currentPitch model.modeSettings.scale model.pitchState)
                    (PitchState.ison model.pitchState.ison)
            )
        , lazy2 backdrop model.menuOpen model.modal
        , lazy header model.headerCollapsed
        , lazy5 viewModal model.audioSettings model.layoutData model.modeSettings model.changelog model.modal

        -- , viewIf LayoutData.showSpacing (div [ class "text-center" ] [ text "|" ])
        , viewIf model.menuOpen menu
        , main_
            [ class "lg:container lg:mx-auto font-serif"
            , case layoutFor model.layoutData of
                Vertical ->
                    class "flex flex-col md:flex-row flex-wrap pb-8"

                Horizontal ->
                    Styles.flexCol
            , Html.Events.on "keydown" keyDecoder
            , Attr.attributeIf model.menuOpen (onClick ToggleMenu)
            ]
            [ lazy5 PitchSpace.view
                model.pitchSpaceData
                model.audioSettings
                model.modeSettings
                model.pitchState
                model.detectedPitch
            , View.Controls.view
                model.audioSettings
                model.modeSettings
                model.pitchState
                model.openControlMenus
            , lazy View.Controls.viewOverlay model.openControlMenus
            , lazy2 pitchTracker model.audioSettings model.layoutData
            ]
        ]


backdrop : Bool -> Modal -> Html Msg
backdrop menuOpen modal =
    let
        show =
            menuOpen || Model.modalOpen modal
    in
    div
        [ class "fixed top-0 left-0 w-full h-full"
        , Styles.transition
        , classList
            [ ( "-z-10", not show )
            , ( "bg-slate-400 opacity-40 z-30", show )
            ]
        , Attr.attributeIf menuOpen (onClick ToggleMenu)
        , Attr.attributeIf (Model.modalOpen modal) (onClick (SelectModal NoModal))
        ]
        []


chantEngineNode : AudioSettings -> Scale -> Maybe Pitch -> Maybe Pitch -> Html msg
chantEngineNode audioSettings scale currentPitch currentIson =
    let
        frequency pitch =
            Frequency.frequency audioSettings.pitchStandard
                audioSettings.playbackRegister
                (Pitch.position scale pitch)
                |> Frequency.preciseString
    in
    Html.node "chant-engine"
        [ audioSettings.gain
            |> String.fromFloat
            |> Attr.attribute "gain"
        , Maybe.unwrap Attr.empty
            (Attr.attribute "melos" << frequency)
            currentPitch
        , Maybe.unwrap Attr.empty
            (Attr.attribute "ison" << frequency)
            currentIson
        ]
        []


{-| Collapsible header with animated caret button.

Caret points down when expanded, rotates 180° when collapsed.

-}
header : Bool -> Html Msg
header headerCollapsed =
    Html.header
        [ Styles.flexRowCentered
        , Styles.transition
        , class "px-2 md:px-4"
        , classList
            [ ( "py-1", headerCollapsed )
            , ( "py-4", not headerCollapsed )
            ]
        ]
        [ div [ class "w-7" ]
            [ button
                [ class "w-full lg:hidden"
                , Styles.transition
                , classList [ ( "-rotate-90", headerCollapsed ) ]
                , onClick ToggleHeaderCollapsed
                ]
                [ Icons.caretDown [ Svg.Attributes.class "w-6 h-6" ]
                ]
            ]
        , div
            [ class "grid ease-in-out flex-1 mx-4"
            , Styles.transition
            , classList
                [ ( "grid-rows-[0fr]", headerCollapsed )
                , ( "grid-rows-[1fr]", not headerCollapsed )
                ]
            ]
            [ div
                [ class "overflow-hidden"
                , Styles.flexCol
                , class "mb-4"
                ]
                [ h1 [ class "font-heading text-4xl text-center" ]
                    [ text "ByzanTone" ]
                , p [ class "font-serif text-center" ]
                    [ text "A tool for learning the pitches and intervals of Byzantine chant." ]

                -- , viewIf model.layoutData.showSpacing (p [ class "text-center" ] [ text "|" ])
                ]
            ]
        , button
            [ class "w-7 mt-2 self-start"
            , onClick ToggleMenu
            ]
            [ Icons.bars [ Svg.Attributes.fill "currentColor", Svg.Attributes.class "w-6 h-6" ] ]
        ]


{-| TODO: give this a drawer effect
-}
menu : Html Msg
menu =
    let
        menuItem modal =
            Html.li []
                [ button
                    [ class "p-2 hover:bg-gray-200 w-full"
                    , Styles.transition
                    , onClick (SelectModal modal)
                    ]
                    [ text (Model.modalToString modal) ]
                ]
    in
    Html.ul
        [ class "fixed top-0 right-0 z-50 bg-white shadow-md"
        , Styles.borderRounded
        , class "font-serif"
        , id "menu"
        , Html.Events.on "keydown" keyDecoder
        ]
        [ menuItem AboutModal
        , menuItem (ModeModal Nothing)
        , menuItem SettingsModal
        , menuItem (ReleasesModal False)
        ]


viewModal : AudioSettings -> LayoutData -> ModeSettings -> RemoteData Http.Error Changelog -> Modal -> Html Msg
viewModal audioSettings layoutData modeSettings changelog modal =
    case modal of
        NoModal ->
            Html.Extra.nothing

        _ ->
            Html.node "dialog"
                [ class "absolute"
                , class "top-8 lg:inset-1/5"
                , class "max-h-5/6 lg:max-h-3/5"
                , class "max-w-7/8 mx-auto"
                , class "overflow-auto"
                , Styles.flexCol
                , class "z-40"
                , class "px-4 sm:px-6 pb-6 bg-white"
                , Styles.borderRounded
                , class "shadow-md font-serif"
                , id "modal"
                ]
                [ h2
                    [ Styles.flexRow
                    , class "justify-between items-center mb-1"
                    , class "sticky top-0 bg-white py-4"
                    ]
                    [ span [ class "font-heading text-2xl" ]
                        [ text (Model.modalToString modal) ]
                    , button
                        [ class "w-8 p-2 cursor-pointer"
                        , onClick (SelectModal NoModal)
                        ]
                        [ Icons.xmark [ Svg.Attributes.fill "currentColor", Svg.Attributes.class "w-6 h-6" ] ]
                    ]
                , modalContent audioSettings layoutData modeSettings changelog modal
                ]


modalContent : AudioSettings -> LayoutData -> ModeSettings -> RemoteData Http.Error Changelog -> Modal -> Html Msg
modalContent audioSettings layoutData modeSettings changelog modal =
    case modal of
        NoModal ->
            Html.Extra.nothing

        AboutModal ->
            Copy.about

        ModeModal maybeMode ->
            lazy viewModeModal maybeMode

        SettingsModal ->
            lazy3 settings audioSettings layoutData modeSettings

        ReleasesModal viewAll ->
            lazy2 View.Changelog.view viewAll changelog


settings : AudioSettings -> LayoutData -> ModeSettings -> Html Msg
settings { pitchStandard } layoutData modeSettings =
    div [ Styles.flexCol, class "gap-2" ]
        [ lazy2 RadioFieldset.view layoutRadioConfig layoutData.layoutSelection
        , RadioFieldset.view (pitchStandardRadioConfig pitchStandard) pitchStandard
        , lazy rangeFieldset modeSettings
        ]


layoutRadioConfig : RadioFieldset.Config LayoutSelection Msg
layoutRadioConfig =
    RadioFieldset.baseConfig
        { itemToString = LayoutData.layoutString
        , legendText = "Layout"
        , onSelect = SetLayout
        , options = [ Auto, Manual Vertical, Manual Horizontal ]
        }


pitchStandardRadioConfig : PitchStandard -> RadioFieldset.Config PitchStandard Msg
pitchStandardRadioConfig currentPitchStandard =
    let
        isSelectedEqualityCheck =
            case currentPitchStandard of
                VariableDi _ ->
                    \option _ ->
                        case option of
                            VariableDi _ ->
                                True

                            _ ->
                                False

                _ ->
                    (==)
    in
    RadioFieldset.baseConfig
        { itemToString = Frequency.pitchStandardToString
        , legendText = "Pitch Standard"
        , onSelect = SetPitchStandard
        , options = [ Ni256, Ke440, VariableDi (Frequency.pitchStandardToDiFrequency currentPitchStandard) ]
        }
        |> RadioFieldset.withCustomViewItem viewPitchStandard
        |> RadioFieldset.withCustomSelected isSelectedEqualityCheck
        |> RadioFieldset.withConditionalPostpend
            (\selectedPitchStandard ->
                case selectedPitchStandard of
                    VariableDi someFreq ->
                        variableDiFrequencyInput someFreq

                    _ ->
                        Html.Extra.nothing
            )


variableDiFrequencyInput : Frequency -> Html Msg
variableDiFrequencyInput diFrequency =
    let
        input type_ attrs =
            Html.input
                ([ Attr.type_ type_
                 , Attr.id ("variable_di_" ++ type_)
                 , Attr.min "300"
                 , Attr.max "480"
                 , Attr.step "0.1"
                 , Attr.value (Frequency.preciseString diFrequency)
                 , onInput setPitchStandard
                 ]
                    ++ attrs
                )
                []

        setPitchStandard string =
            String.toFloat string
                |> Maybe.unwrap diFrequency Frequency
                |> VariableDi
                |> SetPitchStandard
    in
    div [ Styles.flexCol, class "mx-8 my-2 gap-2" ]
        [ p [ class "max-w-xs italic text-sm" ]
            [ text "Priest (or cantor) not much one for strict adherance to a pitch standard? "
            , text "Set an arbitrary frequency for a reference "
            , span [ class "font-greek" ] [ text "Δι" ]
            , text " here."
            ]
        , div [ Styles.flexRow ]
            [ input "number" [ class "text-right" ]
            , span [ class "mr-4" ] [ text "Hz" ]
            , input "range" [ class "w-full max-w-64" ]
            ]
        ]


{-| Set the visible range of the pitch space. A minimum of a tetrachord is
required. Data validation is handled through the input options rather than
through the update logic; we may need to consider additional validation
depending on how dynamic we want to be, and on how well we should reflect
specific modal constraints.
-}
rangeFieldset : ModeSettings -> Html Msg
rangeFieldset { rangeStart, rangeEnd } =
    let
        maxLowerBound =
            Degree.step rangeEnd -3 |> Maybe.withDefault rangeStart

        minUpperBound =
            Degree.step rangeStart 3 |> Maybe.withDefault rangeEnd

        intString degree =
            Degree.indexOf degree |> String.fromInt

        row label id_ min max value msg =
            div [ Styles.flexRow, class "align-center max-w-full flex-wrap" ]
                [ Html.label
                    [ Attr.for id_
                    , class "w-14 mt-1"
                    ]
                    [ text label ]
                , div [ Styles.flexCol ]
                    [ Html.input
                        [ type_ "range"
                        , id id_
                        , class "w-72 md:w-80"
                        , onInput msg
                        , Attr.list (id_ ++ "-datalist")
                        , Attr.min (intString min)
                        , Attr.max (intString max)
                        , Attr.value (intString value)
                        ]
                        []
                    , datalist
                        [ id (id_ ++ "-datalist")
                        , Styles.flexRow
                        , class "justify-between w-72 md:w-80"
                        ]
                        (Degree.range min max |> List.map (makeOption msg))
                    ]
                ]

        makeOption msg degree =
            Html.option
                [ Attr.value (intString degree)
                , Attr.attribute "label" (Degree.toStringGreek degree)
                , onClick (msg (intString degree))
                , class "cursor-pointer font-greek"
                ]
                []
    in
    Html.fieldset [ Styles.borderRounded, class "px-2 pb-1 mb-2" ]
        [ Html.legend [ class "px-1" ] [ Html.text "Range" ]
        , row "From:" "range-start" GA maxLowerBound rangeStart SetRangeStart
        , row "To:" "range-end" minUpperBound Ga_ rangeEnd SetRangeEnd
        ]


viewPitchStandard : PitchStandard -> Html msg
viewPitchStandard pitchStandard =
    let
        ( martyria, frequency ) =
            case pitchStandard of
                Ni256 ->
                    ( Martyria.for Diatonic Ni |> ByzHtmlMartyria.view
                    , " = 256 Hz"
                    )

                Ke440 ->
                    ( Martyria.for Diatonic Ke |> ByzHtmlMartyria.view
                    , " = 440 Hz"
                    )

                VariableDi _ ->
                    ( Martyria.for Diatonic Di |> ByzHtmlMartyria.view
                    , " = Priest’s Special"
                    )
    in
    div [ class "mb-1" ]
        [ span [ class "text-xl relative bottom-1.5" ] [ martyria ]
        , text frequency
        ]


viewModeModal : Maybe Mode -> Html Msg
viewModeModal maybeMode =
    case maybeMode of
        Nothing ->
            RadioFieldset.view modeDataRadioConfig maybeMode

        Just mode ->
            View.ModeData.view mode


modeDataRadioConfig : RadioFieldset.Config (Maybe Mode) Msg
modeDataRadioConfig =
    RadioFieldset.baseConfig
        { itemToString = Maybe.unwrap "" Mode.toString
        , legendText = "Mode"
        , onSelect = SelectModal << ModeModal
        , options = List.map Just Mode.all
        }


pitchTracker : AudioSettings -> LayoutData -> Html Msg
pitchTracker audioSettings layoutData =
    case audioSettings.audioMode of
        AudioSettings.Listen ->
            let
                -- Disable visualization on small screens for performance
                shouldRenderVisualization =
                    layoutData.viewport.viewport.width >= 640
            in
            div [ class "m-4" ]
                [ Html.node "pitch-tracker"
                    [ Attr.attribute "smoothing"
                        (case audioSettings.responsiveness of
                            AudioSettings.Sensitive ->
                                "sensitive"

                            AudioSettings.Smooth ->
                                "smooth"
                        )
                    , Attr.attribute "render-visualization"
                        (if shouldRenderVisualization then
                            "true"

                         else
                            "false"
                        )
                    , class "hidden sm:block"
                    ]
                    []
                ]

        AudioSettings.Play ->
            Html.Extra.nothing



-- HELPERS


keyDecoder : Decoder Msg
keyDecoder =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.map Keydown
