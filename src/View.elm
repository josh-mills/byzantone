module View exposing (view)

import Byzantine.Accidental as Accidental
import Byzantine.ByzHtml.Accidental as Accidental
import Byzantine.ByzHtml.Martyria as Martyria
import Byzantine.Degree as Degree exposing (Degree(..))
import Byzantine.Frequency as Frequency exposing (Frequency, PitchStandard(..))
import Byzantine.IntervalCharacter exposing (..)
import Byzantine.Martyria as Martyria
import Byzantine.Pitch as Pitch exposing (Pitch)
import Byzantine.Register as Register exposing (Register(..))
import Byzantine.Scale as Scale exposing (Scale(..))
import Copy
import Html exposing (Html, button, datalist, div, h1, h2, input, main_, p, span, text)
import Html.Attributes as Attr exposing (class, classList, id, type_)
import Html.Attributes.Extra as Attr
import Html.Events exposing (onClick, onInput)
import Html.Extra exposing (viewIf)
import Html.Lazy exposing (lazy, lazy2, lazy3, lazy4, lazy5)
import Icons
import Json.Decode exposing (Decoder)
import List.Extra as List
import Maybe.Extra as Maybe
import Model exposing (Modal(..), Model)
import Model.AudioSettings as AudioSettings exposing (AudioSettings)
import Model.LayoutData as LayoutData exposing (Layout(..), LayoutData, LayoutSelection(..), layoutFor)
import Model.ModeSettings exposing (ModeSettings)
import Model.PitchState as PitchState exposing (IsonStatus, PitchState)
import Movement exposing (Movement(..))
import RadioFieldset
import Styles
import Svg.Attributes
import Update exposing (Msg(..))
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
        , header
        , lazy4 viewModal model.audioSettings model.layoutData model.modeSettings model.modal

        -- , viewIf model.layoutData.showSpacing (div [ class "text-center" ] [ text "|" ])
        , viewIf model.menuOpen menu
        , main_
            [ class "lg:container lg:mx-auto font-serif"
            , case layoutFor model.layoutData of
                Vertical ->
                    class "flex flex-row flex-wrap"

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
            , lazy4 viewControls model.audioSettings model.modeSettings model.pitchState model.detectedPitch
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
            , ( "bg-slate-400 opacity-40 z-10", show )
            ]
        , Attr.attributeIf menuOpen (onClick ToggleMenu)
        , Attr.attributeIf (Model.modalOpen modal) (onClick (SelectModal NoModal))
        ]
        []


chantEngineNode : AudioSettings -> Scale -> Maybe Pitch -> Maybe Pitch -> Html msg
chantEngineNode audioSettings scale currentPitch currentIson =
    let
        frequency pitch =
            Pitch.getPitchFrequency audioSettings.pitchStandard
                audioSettings.playbackRegister
                scale
                pitch
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


header : Html Msg
header =
    Html.header [ Styles.flexRowCentered ]
        [ div [ class "w-7" ] []
        , div [ Styles.flexCol, class "flex-1 mb-4 mx-4" ]
            [ h1 [ class "font-heading text-4xl text-center" ]
                [ text "ByzanTone" ]
            , p [ class "font-serif text-center" ]
                [ text "A tool for learning the pitches and intervals of Byzantine chant." ]

            -- , viewIf model.layoutData.showSpacing (p [ class "text-center" ] [ text "|" ])
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
        , menuItem SettingsModal
        ]


{-| TODO: positioning still needs work.
-}
viewModal : AudioSettings -> LayoutData -> ModeSettings -> Modal -> Html Msg
viewModal audioSettings layoutData modeSettings modal =
    case modal of
        NoModal ->
            Html.Extra.nothing

        _ ->
            Html.node "dialog"
                [ class "fixed inset-1/4 top-24 w-3/4 md:w-1/2 z-10"
                , Styles.flexCol
                , class "p-6 bg-white"
                , Styles.borderRounded
                , class "shadow-md font-serif"
                , id "modal"
                ]
                [ h2 [ Styles.flexRow, class "justify-between mb-1" ]
                    [ span [ class "font-heading text-2xl" ]
                        [ text (Model.modalToString modal) ]
                    , button
                        [ class "w-8 p-2"
                        , onClick (SelectModal NoModal)
                        ]
                        [ Icons.xmark [ Svg.Attributes.fill "currentColor", Svg.Attributes.class "w-6 h-6" ] ]
                    ]
                , modalContent audioSettings layoutData modeSettings modal
                ]


modalContent : AudioSettings -> LayoutData -> ModeSettings -> Modal -> Html Msg
modalContent audioSettings layoutData modeSettings modal =
    case modal of
        NoModal ->
            Html.Extra.nothing

        AboutModal ->
            Copy.about

        SettingsModal ->
            lazy3 settings audioSettings layoutData modeSettings


settings : AudioSettings -> LayoutData -> ModeSettings -> Html Msg
settings audioSettings layoutData modeSettings =
    div [ Styles.flexCol, class "gap-2" ]
        [ lazy2 RadioFieldset.view layoutRadioConfig layoutData.layoutSelection
        , lazy2 RadioFieldset.view
            (registerRadioConfig "Playback Register" SetPlaybackRegister)
            audioSettings.playbackRegister
        , lazy2 RadioFieldset.view pitchStandardRadioConfig audioSettings.pitchStandard
        , lazy gainInput audioSettings
        , lazy rangeFieldset modeSettings
        ]


layoutRadioConfig : RadioFieldset.Config LayoutSelection Msg
layoutRadioConfig =
    { itemToString = LayoutData.layoutString
    , legendText = "Layout"
    , onSelect = SetLayout
    , options = [ Auto, Manual Vertical, Manual Horizontal ]
    , viewItem = Nothing
    }


registerRadioConfig : String -> (Register -> Msg) -> RadioFieldset.Config Register Msg
registerRadioConfig legendText onSelect =
    { itemToString = Register.toString
    , legendText = legendText
    , onSelect = onSelect
    , options = [ Treble, Bass ]
    , viewItem = Nothing
    }


responsivenessRadioConfig : RadioFieldset.Config AudioSettings.Responsiveness Msg
responsivenessRadioConfig =
    { itemToString = AudioSettings.responsivenessToString
    , legendText = "Responsiveness"
    , onSelect = SetResponsiveness
    , options = [ AudioSettings.Sensitive, AudioSettings.Smooth ]
    , viewItem = Nothing
    }


pitchStandardRadioConfig : RadioFieldset.Config PitchStandard Msg
pitchStandardRadioConfig =
    { itemToString = Frequency.pitchStandardToString
    , legendText = "Pitch Standard"
    , onSelect = SetPitchStandard
    , options = [ Ni256, Ke440 ]
    , viewItem = Just viewPitchStandard
    }


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
            div [ Styles.flexRow, class "align-center" ]
                [ Html.label
                    [ Attr.for id_
                    , class "w-14 mt-1"
                    ]
                    [ text label ]
                , div [ Styles.flexCol ]
                    [ Html.input
                        [ type_ "range"
                        , id id_
                        , class "w-80"
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
                        , class "justify-between w-80"
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
                    ( Martyria.for Diatonic Ni |> Martyria.view
                    , " = 256 Hz"
                    )

                Ke440 ->
                    ( Martyria.for Diatonic Ke |> Martyria.view
                    , " = 440 Hz"
                    )
    in
    div [ class "mb-1" ]
        [ span [ class "text-xl relative bottom-1.5" ] [ martyria ]
        , text frequency
        ]



-- CONTROLS


viewControls : AudioSettings -> ModeSettings -> PitchState -> Maybe Frequency -> Html Msg
viewControls audioSettings modeSettings pitchState detectedPitch =
    div [ class "w-max", classList [ ( "    mt-8", LayoutData.showSpacing ) ] ]
        [ lazy2 RadioFieldset.view scaleRadioConfig modeSettings.scale
        , lazy2 RadioFieldset.view playModeRadioConfig audioSettings.audioMode
        , case audioSettings.audioMode of
            AudioSettings.Listen ->
                div []
                    [ lazy2 RadioFieldset.view
                        (registerRadioConfig "Listen Register" SetListenRegister)
                        audioSettings.listenRegister
                    , lazy2 RadioFieldset.view responsivenessRadioConfig audioSettings.responsiveness
                    , Html.node "pitch-tracker"
                        [ Attr.attribute "smoothing"
                            (case audioSettings.responsiveness of
                                AudioSettings.Sensitive ->
                                    "sensitive"

                                AudioSettings.Smooth ->
                                    "smooth"
                            )
                        , class "hidden sm:block"
                        ]
                        []
                    ]

            AudioSettings.Play ->
                div []
                    [ lazy isonButton pitchState.ison
                    , lazy viewIson (PitchState.ison pitchState.ison)
                    , lazy viewCurrentPitch (PitchState.currentPitch modeSettings.scale pitchState)
                    , lazy gainInput audioSettings
                    ]
        ]


scaleRadioConfig : RadioFieldset.Config Scale Msg
scaleRadioConfig =
    { itemToString = Scale.name
    , legendText = "Select Scale"
    , onSelect = SetScale
    , options = Scale.all
    , viewItem = Nothing
    }


playModeRadioConfig : RadioFieldset.Config AudioSettings.AudioMode Msg
playModeRadioConfig =
    { itemToString = AudioSettings.audioModeToString
    , legendText = "Audio Mode"
    , onSelect = SetAudioMode
    , options = AudioSettings.modes
    , viewItem = Nothing
    }


isonButton : IsonStatus -> Html Msg
isonButton ison =
    button
        [ Styles.buttonClass
        , class "my-2"
        , id "select-ison-button"
        , onClick
            (SetIson
                (case ison of
                    PitchState.NoIson ->
                        PitchState.SelectingIson Nothing

                    PitchState.SelectingIson (Just ison_) ->
                        PitchState.Selected ison_

                    PitchState.SelectingIson Nothing ->
                        PitchState.NoIson

                    PitchState.Selected _ ->
                        PitchState.SelectingIson
                            (PitchState.ison ison
                                |> Maybe.map Pitch.unwrapDegree
                            )
                )
            )
        ]
        [ text "Select Ison" ]


viewIson : Maybe Pitch -> Html Msg
viewIson pitch =
    div [ class "mt-2" ]
        [ text <| "Current Ison: "
        , case pitch of
            Nothing ->
                text "none"

            Just p ->
                Degree.text (Pitch.unwrapDegree p)
        , viewIf (Maybe.isJust pitch) (clearButton (SetIson PitchState.NoIson))
        ]


viewCurrentPitch : Maybe Pitch -> Html Msg
viewCurrentPitch pitch =
    div [ class "mt-2" ]
        [ text <| "Current Pitch: "
        , case pitch of
            Nothing ->
                text "none"

            Just pitch_ ->
                span []
                    [ Degree.text (Pitch.unwrapDegree pitch_)
                    , Html.Extra.viewMaybe
                        (\accidental ->
                            span []
                                [ text " ("
                                , text (Accidental.toString accidental)
                                , text ")"
                                ]
                        )
                        (Pitch.unwrapAccidental pitch_)
                    ]
        , viewIf (Maybe.isJust pitch) (clearButton (SelectPitch Nothing Nothing))
        ]


clearButton : Msg -> Html Msg
clearButton msg =
    button
        [ Styles.buttonClass
        , class "my-2 mx-2"
        , onClick msg
        ]
        [ text "clear" ]


gainInput : AudioSettings -> Html Msg
gainInput { gain } =
    let
        ( buttonText, msg ) =
            if gain > 0 then
                ( "mute", SetGain 0 )

            else
                ( "unmute", SetGain 0.2 )
    in
    div []
        [ button
            [ Styles.buttonClass
            , class "w-24 my-2 mr-4"
            , onClick msg
            ]
            [ text buttonText ]
        , input
            [ type_ "range"
            , Attr.min "0"
            , Attr.max "1"
            , Attr.step "0.02"
            , Attr.value <| String.fromFloat gain
            , onInput (SetGain << Maybe.withDefault gain << String.toFloat)
            ]
            []
        ]



-- HELPERS


keyDecoder : Decoder Msg
keyDecoder =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.map Keydown
