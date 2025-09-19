module View.Controls exposing (view)

import Byzantine.ByzHtml.Interval as ByzHtmlInterval
import Byzantine.Degree as Degree
import Byzantine.IntervalCharacter as Character
import Byzantine.Pitch as Pitch exposing (Pitch)
import Byzantine.Register as Register exposing (Register)
import Byzantine.Scale as Scale exposing (Scale(..))
import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr exposing (class, classList, id)
import Html.Events exposing (onClick, onInput)
import Html.Extra exposing (viewIf)
import Html.Lazy exposing (..)
import Icons
import Maybe.Extra as Maybe
import Model.AudioSettings as AudioSettings exposing (AudioSettings)
import Model.ControlsMenu as ControlsMenu exposing (MenuOption(..), OpenControlMenus)
import Model.ModeSettings exposing (ModeSettings)
import Model.PitchState as PitchState exposing (IsonStatus, PitchState)
import RadioFieldset
import Styles
import Svg
import Svg.Attributes as Svg
import Update exposing (Msg(..))


view : AudioSettings -> ModeSettings -> PitchState -> OpenControlMenus -> Html Msg
view audioSettings modeSettings pitchState openControlMenus =
    Html.menu [ class "w-full sm:w-72" ]
        (List.map
            (lazy5 item audioSettings modeSettings pitchState openControlMenus)
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
        [ lazy3 optionHeader audioSettings isOpen menuOption
        , optionContent audioSettings modeSettings pitchState isOpen menuOption
        ]


type IconType msg
    = SvgIcon (List (Svg.Attribute msg) -> Html msg)
    | ByzHtmlIcon (Html msg)


optionHeader : AudioSettings -> Bool -> MenuOption -> Html Msg
optionHeader audioSettings isOpen menuOption =
    Html.button
        [ class "w-full h-12"
        , Styles.buttonClass
        , Styles.border
        , Styles.transitionQuick
        , classList [ ( "rounded-b-none", isOpen ) ]
        , onClick (Update.ToggleControlMenu menuOption)
        ]
        [ case menuOption of
            AudioModeMenu ->
                optionHeaderWrapper isOpen
                    "Audio"
                    (case audioSettings.audioMode of
                        AudioSettings.Listen ->
                            SvgIcon Icons.microphone

                        AudioSettings.Play ->
                            SvgIcon Icons.headphones
                    )

            AudioSettingsMenu ->
                optionHeaderWrapper isOpen "Audio Settings" (SvgIcon Icons.sliders)

            IsonMenu ->
                optionHeaderWrapper isOpen
                    "Ison"
                    (ByzHtmlIcon (ByzHtmlInterval.view Character.Ison))

            ScaleMenu ->
                optionHeaderWrapper isOpen "Scale" (SvgIcon Icons.music)

            VolumeMenu ->
                optionHeaderWrapper isOpen
                    "Volume"
                    (if audioSettings.gain == 0 then
                        SvgIcon Icons.volumeOff

                     else if audioSettings.gain <= 0.5 then
                        SvgIcon Icons.volumeLow

                     else
                        SvgIcon Icons.volumeHigh
                    )
        ]


optionHeaderWrapper : Bool -> String -> IconType Msg -> Html Msg
optionHeaderWrapper isOpen optionHeaderText icon =
    div [ Styles.flexRow, class "justify-between" ]
        [ div [ Styles.flexRow ]
            [ case icon of
                SvgIcon svg ->
                    svg [ Svg.fill "grey", Svg.width "24" ]

                ByzHtmlIcon html ->
                    span [ class "text-neutral-500" ] [ html ]
            , span [ class "ml-2" ] [ text optionHeaderText ]
            ]
        , div
            [ class "w-6 transition-transform duration-300 ease-in-out"
            , classList [ ( "rotate-180", isOpen ) ]
            ]
            [ Icons.chevronDown [ Svg.fill "grey" ] ]
        ]


optionContent : AudioSettings -> ModeSettings -> PitchState -> Bool -> MenuOption -> Html Msg
optionContent audioSettings modeSettings pitchState isOpen menuOption =
    let
        wrapper =
            div [ class "m-2" ]
    in
    div
        [ if isOpen then
            Styles.border

          else
            Styles.borderTransparent
        , classList [ ( "mb-2 rounded-b-md", isOpen ) ]
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

            IsonMenu ->
                wrapper
                    [ lazy isonButton pitchState.ison
                    , lazy viewIsonStatus (PitchState.ison pitchState.ison)
                    ]

            ScaleMenu ->
                wrapper [ lazy2 RadioFieldset.view scaleRadioConfig modeSettings.scale ]

            VolumeMenu ->
                wrapper [ lazy gainInput audioSettings ]
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
