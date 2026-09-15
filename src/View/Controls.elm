module View.Controls exposing (view, viewOverlay)

import Byzantine.ByzHtml.Interval as ByzHtmlInterval
import Byzantine.ByzHtml.ModalSignature as ByzHtmlMode
import Byzantine.Degree as Degree
import Byzantine.IntervalCharacter as Character
import Byzantine.Pitch as Pitch exposing (Pitch)
import Byzantine.Register as Register exposing (Register)
import Byzantine.Scale as Scale exposing (Scale)
import Components.Collapsible as Collapsible
import Components.RadioFieldset as RadioFieldset
import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr exposing (class, classList, id)
import Html.Events exposing (onClick, onInput)
import Html.Extra exposing (viewIf)
import Html.Lazy exposing (..)
import Icons
import Maybe.Extra as Maybe
import ModeBuilder
import Model.AudioSettings as AudioSettings exposing (AudioSettings, ListenRegister)
import Model.ControlsMenu as ControlsMenu exposing (MenuOption(..), OpenControlMenus)
import Model.LayoutData exposing (Layout(..))
import Model.ModeSettings exposing (ModeSettings)
import Model.PitchState as PitchState exposing (IsonStatus, PitchState)
import Styles
import Svg
import Svg.Attributes as Svg
import Update exposing (Msg(..))


{-| Tap catcher overlay
-}
viewOverlay : Layout -> OpenControlMenus -> Html Msg
viewOverlay layout openControlMenus =
    Html.Extra.viewIf (ControlsMenu.anyOpen openControlMenus)
        (div
            [ class "fixed left-0 top-0 w-full h-full z-10 bg-slate-400 opacity-40"
            , classList [ ( "2xl:hidden", layout == Vertical ) ]
            , onClick CloseControlMenus
            ]
            []
        )


view : Bool -> Layout -> AudioSettings -> ModeSettings -> PitchState -> OpenControlMenus -> ModeBuilder.Model -> Html Msg
view devMode layout audioSettings modeSettings pitchState openControlMenus modeBuilderModel =
    let
        menuItem isOpen menuOption =
            item layout audioSettings modeSettings pitchState isOpen modeBuilderModel menuOption
    in
    Html.menu
        (case layout of
            Horizontal ->
                [ class "w-full grid grid-cols-6 fixed bottom-0 left-0 z-20" ]

            Vertical ->
                [ class "w-full 2xl:w-72"
                , class "grid grid-cols-6 2xl:flex 2xl:flex-col"
                , class "fixed bottom-0 left-0 2xl:relative 2xl:left-auto"
                , class "z-20"
                ]
        )
        [ menuItem openControlMenus.audioModeIsOpen AudioModeMenu
        , menuItem openControlMenus.audioSettingsMenuIsOpen AudioSettingsMenu
        , menuItem openControlMenus.isonMenuIsOpen IsonMenu
        , menuItem openControlMenus.scaleMenuIsOpen ScaleMenu
        , viewIf devMode (menuItem openControlMenus.modeBuilderMenuIsOpen ModeBuilderMenu)
        , menuItem openControlMenus.volumeMenuIsOpen VolumeMenu
        ]


item : Layout -> AudioSettings -> ModeSettings -> PitchState -> Bool -> ModeBuilder.Model -> MenuOption -> Html Msg
item layout audioSettings modeSettings pitchState isOpen modeBuilderModel menuOption =
    (Collapsible.isOpen isOpen
        |> Collapsible.withFirstChildTrigger
        |> Collapsible.withTransition Collapsible.TransitionQuick
        |> Collapsible.li
    )
        []
        [ lazy4 optionHeader layout audioSettings isOpen menuOption
        , optionContent layout audioSettings modeSettings pitchState modeBuilderModel isOpen menuOption
        ]


type IconType msg
    = SvgIcon (List (Svg.Attribute msg) -> Html msg)
    | ByzHtmlIcon (Html msg)


optionHeader : Layout -> AudioSettings -> Bool -> MenuOption -> Html Msg
optionHeader layout audioSettings isOpen menuOption =
    Html.button
        ([ class "w-full min-h-12 py-2 px-3 hover:bg-gray-300"
         , onClick (Update.ToggleControlMenu menuOption)
         ]
            ++ (case layout of
                    Horizontal ->
                        [ class "bg-white border-t border-gray-300" ]

                    Vertical ->
                        [ class "bg-white 2xl:bg-gray-200"
                        , class "border-t 2xl:border border-gray-300 2xl:rounded-md"
                        , classList [ ( "2xl:rounded-b-none", isOpen ) ]
                        ]
               )
        )
        [ case menuOption of
            AudioModeMenu ->
                optionHeaderWrapper layout
                    isOpen
                    [ text "Audio"
                    , span [ class "hidden sm:inline" ] [ text " Mode" ]
                    ]
                    (case audioSettings.audioMode of
                        AudioSettings.Listen ->
                            SvgIcon Icons.microphone

                        AudioSettings.Play ->
                            SvgIcon Icons.headphones
                    )

            AudioSettingsMenu ->
                optionHeaderWrapper layout
                    isOpen
                    [ span [ class "hidden sm:inline" ] [ text "Audio " ]
                    , text "Settings"
                    ]
                    (SvgIcon Icons.sliders)

            IsonMenu ->
                optionHeaderWrapper layout
                    isOpen
                    [ text "Ison" ]
                    (ByzHtmlIcon (ByzHtmlInterval.view Character.Ison))

            ScaleMenu ->
                optionHeaderWrapper layout
                    isOpen
                    [ text "Scale" ]
                    (SvgIcon Icons.music)

            ModeBuilderMenu ->
                optionHeaderWrapper layout
                    isOpen
                    [ text "Mode" ]
                    (ByzHtmlIcon ByzHtmlMode.modeWordEchos)

            VolumeMenu ->
                optionHeaderWrapper layout
                    isOpen
                    [ text "Volume" ]
                    (if audioSettings.gain == 0 then
                        SvgIcon Icons.volumeOff

                     else if audioSettings.gain <= 0.5 then
                        SvgIcon Icons.volumeLow

                     else
                        SvgIcon Icons.volumeHigh
                    )
        ]


optionHeaderWrapper : Layout -> Bool -> List (Html Msg) -> IconType Msg -> Html Msg
optionHeaderWrapper layout isOpen optionHeaderTextNodes icon =
    div
        [ Styles.flexRow
        , class
            (case layout of
                Horizontal ->
                    "justify-around"

                Vertical ->
                    "justify-around 2xl:justify-between"
            )
        ]
        [ div
            [ class "flex flex-col md:flex-row" ]
            [ case icon of
                SvgIcon svg ->
                    div [ class "mx-auto self-center" ]
                        [ svg [ Svg.fill "grey", Svg.width "24" ] ]

                ByzHtmlIcon html ->
                    div [ class "text-neutral-500 self-center" ] [ html ]
            , div [ class "md:ml-2 text-xs md:text-base" ]
                optionHeaderTextNodes
            ]
        , viewIf (layout == Vertical)
            (div
                [ class "hidden 2xl:block w-6"
                , Styles.transitionQuick
                , classList [ ( "rotate-180", isOpen ) ]
                ]
                [ Icons.chevronDown [ Svg.fill "grey" ] ]
            )
        ]


optionContent : Layout -> AudioSettings -> ModeSettings -> PitchState -> ModeBuilder.Model -> Bool -> MenuOption -> Html Msg
optionContent layout audioSettings modeSettings pitchState modeBuilderModel isOpen menuOption =
    let
        wrapper =
            div
                [ Styles.flexCol
                , class
                    (case layout of
                        Horizontal ->
                            "max-w-sm mx-auto"

                        Vertical ->
                            "max-w-sm mx-auto 2xl:m-2 2xl:max-w-none"
                    )
                ]
    in
    div
        ([ if isOpen then
            Styles.border

           else
            Styles.borderTransparent
         , class "overflow-hidden bg-white"
         , Styles.transitionQuick
         , Attr.attribute "aria-hidden"
            (if isOpen then
                "false"

             else
                "true"
            )
         ]
            ++ (case layout of
                    Horizontal ->
                        [ class "fixed w-full left-0 bottom-0 z-30 px-4 py-2"
                        , classList
                            [ ( "translate-y-full", not isOpen )
                            , ( "translate-y-0", isOpen )
                            ]
                        ]

                    Vertical ->
                        [ class "fixed 2xl:static w-full left-0 bottom-0 z-30 px-4 py-2 2xl:p-0"
                        , classList
                            [ ( "translate-y-full 2xl:translate-y-0", not isOpen )
                            , ( "translate-y-0 2xl:mb-2 2xl:rounded-b-md", isOpen )
                            ]
                        ]
               )
        )
        [ case menuOption of
            AudioModeMenu ->
                wrapper
                    [ lazy2 RadioFieldset.view audioModeRadioConfig audioSettings.audioMode ]

            AudioSettingsMenu ->
                wrapper
                    (case audioSettings.audioMode of
                        AudioSettings.Listen ->
                            [ lazy2 RadioFieldset.view
                                (listenRegisterRadioConfig audioSettings)
                                audioSettings.listenRegister
                            , lazy2 RadioFieldset.view responsivenessRadioConfig audioSettings.responsiveness
                            , lazy2 RadioFieldset.view pitchFeedbackRadioConfig audioSettings.pitchFeedback
                            ]

                        AudioSettings.Play ->
                            [ lazy2 RadioFieldset.view
                                playbackRegisterRadioConfig
                                audioSettings.playbackRegister
                            ]
                    )

            IsonMenu ->
                wrapper
                    [ lazy isonButton pitchState.ison
                    , lazy viewIsonStatus (PitchState.ison pitchState.ison)
                    ]

            ScaleMenu ->
                wrapper [ lazy2 RadioFieldset.view scaleRadioConfig modeSettings.scale ]

            ModeBuilderMenu ->
                wrapper [ Html.map ModeBuilderMsg (ModeBuilder.view modeBuilderModel) ]

            VolumeMenu ->
                wrapper [ lazy gainInput audioSettings ]
        ]



-- CONTENT


audioModeRadioConfig : RadioFieldset.Config AudioSettings.AudioMode Msg
audioModeRadioConfig =
    RadioFieldset.baseConfig
        { itemToString = AudioSettings.audioModeToString
        , legendText = "Audio Mode"
        , onSelect = Update.SetAudioMode
        , options = AudioSettings.modes
        }


playbackRegisterRadioConfig : RadioFieldset.Config Register Msg
playbackRegisterRadioConfig =
    RadioFieldset.baseConfig
        { itemToString = Register.toString
        , legendText = "Set Playback Register"
        , onSelect = Update.SetPlaybackRegister
        , options = [ Register.Treble, Register.Bass ]
        }


listenRegisterRadioConfig : AudioSettings -> RadioFieldset.Config ListenRegister Msg
listenRegisterRadioConfig audioSettings =
    RadioFieldset.baseConfig
        { itemToString = AudioSettings.listenRegisterToString
        , legendText = "Listen Register"
        , onSelect = Update.SetListenRegister
        , options =
            [ AudioSettings.Auto (AudioSettings.listenRegister audioSettings)
            , AudioSettings.Manual Register.Treble
            , AudioSettings.Manual Register.Bass
            ]
        }


responsivenessRadioConfig : RadioFieldset.Config AudioSettings.Responsiveness Msg
responsivenessRadioConfig =
    RadioFieldset.baseConfig
        { itemToString = AudioSettings.responsivenessToString
        , legendText = "Listening Sensitivity"
        , onSelect = Update.SetResponsiveness
        , options = [ AudioSettings.Sensitive, AudioSettings.Smooth ]
        }


pitchFeedbackRadioConfig : RadioFieldset.Config AudioSettings.PitchFeedbackUnit Msg
pitchFeedbackRadioConfig =
    RadioFieldset.baseConfig
        { itemToString = AudioSettings.pitchFeedbackUnitToString
        , legendText = "Pitch Feedback"
        , onSelect = Update.SetPitchFeedback
        , options = [ AudioSettings.Cents, AudioSettings.Hz, AudioSettings.Moria ]
        }


isonButton : IsonStatus -> Html Msg
isonButton ison =
    button
        [ Styles.buttonClass
        , class "my-2"
        , id "select-ison-button"
        , onClick
            (Update.SetIson
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


viewIsonStatus : Maybe Pitch -> Html Msg
viewIsonStatus pitch =
    div [ class "mt-2" ]
        [ text "Current Ison: "
        , case pitch of
            Nothing ->
                text "none"

            Just p ->
                Degree.text (Pitch.unwrapDegree p)
        , viewIf (Maybe.isJust pitch) clearIsonButton
        ]


clearIsonButton : Html Msg
clearIsonButton =
    button
        [ Styles.buttonClass
        , class "mx-2"
        , onClick (Update.SetIson PitchState.NoIson)
        ]
        [ text "clear" ]


scaleRadioConfig : RadioFieldset.Config Scale Msg
scaleRadioConfig =
    RadioFieldset.baseConfig
        { itemToString = Scale.name
        , legendText = "Select Scale"
        , onSelect = Update.SetScale
        , options = Scale.all
        }


gainInput : AudioSettings -> Html Msg
gainInput { gain } =
    let
        ( buttonText, msg ) =
            if gain > 0 then
                ( "mute", Update.SetGain 0 )

            else
                ( "unmute", Update.SetGain 0.2 )
    in
    div []
        [ Html.button
            [ Styles.buttonClass
            , class "w-24 my-2 mr-4"
            , onClick msg
            ]
            [ text buttonText ]
        , Html.input
            [ Attr.type_ "range"
            , Attr.min "0"
            , Attr.max "1"
            , Attr.step "0.02"
            , Attr.value (String.fromFloat gain)
            , onInput (Update.SetGain << Maybe.withDefault gain << String.toFloat)
            ]
            []
        ]
