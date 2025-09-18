module View.Controls exposing
    ( gainInput
    , view
    )

import Byzantine.Register as Register exposing (Register)
import Byzantine.Scale as Scale exposing (Scale(..))
import Html exposing (Html, div, span, text)
import Html.Attributes as Attr exposing (class, classList)
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (..)
import Icons
import Model.AudioSettings as AudioSettings exposing (AudioSettings)
import Model.ControlsMenu as ControlsMenu exposing (MenuOption(..), OpenControlMenus)
import Model.ModeSettings exposing (ModeSettings)
import Model.PitchState exposing (PitchState)
import RadioFieldset
import Styles
import Svg.Attributes as Svg
import Update exposing (Msg(..))


view : AudioSettings -> ModeSettings -> PitchState -> OpenControlMenus -> Html Msg
view audioSettings modeSettings pitchState openControlMenus =
    Html.menu [ class "w-full sm:w-72" ]
        (List.map
            (item audioSettings modeSettings pitchState openControlMenus)
            ControlsMenu.menuOptions
        )


item : AudioSettings -> ModeSettings -> PitchState -> OpenControlMenus -> MenuOption -> Html Msg
item audioSettings modeSettings pitchState openControlMenus menuOption =
    let
        isOpen =
            ControlsMenu.isOpen openControlMenus menuOption
    in
    Html.li
        [ class "grid transition-[grid-template-rows] duration-300 ease-in-out"
        , classList
            [ ( "grid-rows-[auto_0fr]", not isOpen )
            , ( "grid-rows-[auto_1fr]", isOpen )
            ]
        ]
        [ optionHeader audioSettings openControlMenus menuOption
        , optionContent audioSettings modeSettings pitchState openControlMenus menuOption
        ]


optionHeader : AudioSettings -> OpenControlMenus -> MenuOption -> Html Msg
optionHeader audioSettings openControlMenus menuOption =
    let
        wrapper headerText icon =
            optionHeaderWrapper
                (ControlsMenu.isOpen openControlMenus menuOption)
                headerText
                (icon [ Svg.fill "grey", Svg.width "24" ])
    in
    Html.button
        [ class "w-full h-12"
        , Styles.buttonClass
        , Styles.border
        , onClick (Update.ToggleControlMenu menuOption)
        ]
        [ case menuOption of
            AudioModeMenu ->
                wrapper "Audio"
                    (case audioSettings.audioMode of
                        AudioSettings.Listen ->
                            Icons.microphone

                        AudioSettings.Play ->
                            Icons.headphones
                    )

            AudioSettingsMenu ->
                wrapper "Audio Settings" Icons.sliders

            ScaleMenu ->
                wrapper "Scale" Icons.music

            VolumeMenu ->
                wrapper "Volume"
                    (if audioSettings.gain == 0 then
                        Icons.volumeOff

                     else if audioSettings.gain <= 0.5 then
                        Icons.volumeLow

                     else
                        Icons.volumeHigh
                    )
        ]


optionHeaderWrapper : Bool -> String -> Html msg -> Html msg
optionHeaderWrapper isOpen optionHeaderText icon =
    div [ Styles.flexRow, class "justify-between" ]
        [ div [ Styles.flexRow ]
            [ icon
            , span [ class "ml-2" ] [ text optionHeaderText ]
            ]
        , div
            [ class "w-6 transition-transform duration-300 ease-in-out"
            , classList [ ( "rotate-180", isOpen ) ]
            ]
            [ Icons.chevronDown [ Svg.fill "grey" ] ]
        ]


optionContent : AudioSettings -> ModeSettings -> PitchState -> OpenControlMenus -> MenuOption -> Html Msg
optionContent audioSettings modeSettings pitchState openControlMenus menuOption =
    let
        isOpen =
            ControlsMenu.isOpen openControlMenus menuOption

        wrapper =
            div [ class "m-2" ]
    in
    div
        [ if isOpen then
            Styles.border

          else
            Styles.borderTransparent
        , classList [ ( "mb-2", isOpen ) ]
        , class "overflow-hidden"
        ]
        [ case menuOption of
            AudioModeMenu ->
                wrapper
                    [ lazy2 RadioFieldset.view audioModeRadioConfig audioSettings.audioMode ]

            AudioSettingsMenu ->
                wrapper
                    (case audioSettings.audioMode of
                        AudioSettings.Listen ->
                            [ lazy2 RadioFieldset.view
                                (registerRadioConfig "Listen Register" Update.SetListenRegister)
                                audioSettings.listenRegister
                            , lazy2 RadioFieldset.view responsivenessRadioConfig audioSettings.responsiveness
                            ]

                        AudioSettings.Play ->
                            [ lazy2 RadioFieldset.view
                                (registerRadioConfig "Set Playback Register" Update.SetPlaybackRegister)
                                audioSettings.playbackRegister
                            ]
                    )

            ScaleMenu ->
                wrapper [ lazy2 RadioFieldset.view scaleRadioConfig modeSettings.scale ]

            VolumeMenu ->
                wrapper [ gainInput audioSettings ]
        ]



-- CONTENT


audioModeRadioConfig : RadioFieldset.Config AudioSettings.AudioMode Msg
audioModeRadioConfig =
    { itemToString = AudioSettings.audioModeToString
    , legendText = "Audio Mode"
    , onSelect = Update.SetAudioMode
    , options = AudioSettings.modes
    , viewItem = Nothing
    }


registerRadioConfig : String -> (Register -> Msg) -> RadioFieldset.Config Register Msg
registerRadioConfig legendText onSelect =
    { itemToString = Register.toString
    , legendText = legendText
    , onSelect = onSelect
    , options = [ Register.Treble, Register.Bass ]
    , viewItem = Nothing
    }


responsivenessRadioConfig : RadioFieldset.Config AudioSettings.Responsiveness Msg
responsivenessRadioConfig =
    { itemToString = AudioSettings.responsivenessToString
    , legendText = "Listening Sensitivity"
    , onSelect = Update.SetResponsiveness
    , options = [ AudioSettings.Sensitive, AudioSettings.Smooth ]
    , viewItem = Nothing
    }


scaleRadioConfig : RadioFieldset.Config Scale Msg
scaleRadioConfig =
    { itemToString = Scale.name
    , legendText = "Select Scale"
    , onSelect = Update.SetScale
    , options = Scale.all
    , viewItem = Nothing
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
