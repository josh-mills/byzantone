module View.Controls exposing
    ( gainInput
    , view
    )

import Byzantine.Register as Register exposing (Register)
import Byzantine.Scale as Scale exposing (Scale(..))
import Html exposing (Html, div, text)
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
import Update exposing (Msg(..))


view : AudioSettings -> ModeSettings -> PitchState -> OpenControlMenus -> Html Msg
view audioSettings modeSettings pitchState openControlMenus =
    Html.menu
        [ class "w-full sm:w-64"
        , class ""
        ]
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
        [ optionHeader openControlMenus menuOption
        , optionContent audioSettings modeSettings pitchState openControlMenus menuOption
        ]


optionHeader : OpenControlMenus -> MenuOption -> Html Msg
optionHeader openControlMenus menuOption =
    Html.button
        [ class "w-full h-12"
        , Styles.buttonClass
        , Styles.border
        , onClick (Update.ToggleControlMenu menuOption)
        ]
        [ optionHeaderText openControlMenus menuOption ]


{-| Include icons for these too?
-}
optionHeaderText : OpenControlMenus -> MenuOption -> Html Msg
optionHeaderText openControlMenus menuOption =
    let
        isOpen =
            ControlsMenu.isOpen openControlMenus menuOption
    in
    case menuOption of
        AudioModeMenu ->
            div [ Styles.flexRow, class "justify-between" ]
                [ div [] [ text "Audio" ]
                , div
                    [ class "w-6 transition-transform duration-300 ease-in-out"
                    , classList [ ( "rotate-180", isOpen ) ]
                    ]
                    [ Icons.chevronDown
                        []
                    ]
                ]

        AudioSettingsMenu ->
            div [ Styles.flexRow, class "justify-between" ]
                [ div [] [ text "Audio Settings" ]
                , div
                    [ class "w-6 transition-transform duration-300 ease-in-out"
                    , classList [ ( "rotate-180", isOpen ) ]
                    ]
                    [ Icons.chevronDown
                        []
                    ]
                ]

        ScaleMenu ->
            div [ Styles.flexRow, class "justify-between" ]
                [ div [] [ text "Scale" ]
                , div
                    [ class "w-6 transition-transform duration-300 ease-in-out"
                    , classList [ ( "rotate-180", isOpen ) ]
                    ]
                    [ Icons.chevronDown [] ]
                ]

        VolumeMenu ->
            div [ Styles.flexRow, class "justify-between" ]
                [ div [] [ text "Volume" ]
                , div
                    [ class "w-6 transition-transform duration-300 ease-in-out"
                    , classList [ ( "rotate-180", isOpen ) ]
                    ]
                    [ Icons.chevronDown [] ]
                ]


optionContent : AudioSettings -> ModeSettings -> PitchState -> OpenControlMenus -> MenuOption -> Html Msg
optionContent audioSettings modeSettings pitchState openControlMenus menuOption =
    let
        isOpen =
            ControlsMenu.isOpen openControlMenus menuOption
    in
    div
        [ if isOpen then
            Styles.border

          else
            Styles.borderTransparent
        , classList [ ( "mb-2 -OPEp-2", isOpen ) ]
        , class "overflow-hidden"
        ]
        [ case menuOption of
            AudioModeMenu ->
                div [ class "m-2" ]
                    [ lazy2 RadioFieldset.view audioModeRadioConfig audioSettings.audioMode ]

            AudioSettingsMenu ->
                div [ class "m-2" ]
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
                div [ class "m-2" ]
                    [ lazy2 RadioFieldset.view scaleRadioConfig modeSettings.scale ]

            VolumeMenu ->
                gainInput audioSettings
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
