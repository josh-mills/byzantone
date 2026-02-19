module Model.AudioSettings exposing
    ( AudioSettings, defaultAudioSettings
    , AudioMode(..), audioModeToString, modes
    , ListenRegister(..), listenRegister
    , Responsiveness(..), responsivenessToString
    , PitchFeedback(..), pitchFeedbackToString
    , listenRegisterToString, setAutoListenRegister
    )

{-|

@docs AudioSettings, defaultAudioSettings
@docs AudioMode, audioModeToString, modes
@docs ListenRegister, listenRegister
@docs Responsiveness, responsivenessToString
@docs PitchFeedback, pitchFeedbackToString

-}

import Byzantine.Degree as Degree
import Byzantine.Frequency as Frequency exposing (Frequency, PitchStandard(..))
import Byzantine.PitchPosition as PitchPosition
import Byzantine.Register as Register exposing (Register(..))
import Maybe.Extra
import Model.ModeSettings exposing (ModeSettings)


type alias AudioSettings =
    { gain : Float
    , audioMode : AudioMode
    , pitchStandard : PitchStandard
    , playbackRegister : Register
    , listenRegister : ListenRegister
    , responsiveness : Responsiveness
    , pitchFeedback : PitchFeedback
    }


defaultAudioSettings : AudioSettings
defaultAudioSettings =
    { gain = 0.3
    , audioMode = Play
    , pitchStandard = Ni256
    , playbackRegister = Treble
    , listenRegister = Auto Bass
    , responsiveness = Smooth
    , pitchFeedback = Moria
    }


type AudioMode
    = Play
    | Listen


modes : List AudioMode
modes =
    [ Play, Listen ]


audioModeToString : AudioMode -> String
audioModeToString mode =
    case mode of
        Play ->
            "Play"

        Listen ->
            "Listen"


type ListenRegister
    = Auto Register
    | Manual Register


listenRegister : AudioSettings -> Register
listenRegister audioSettings =
    case audioSettings.listenRegister of
        Auto register ->
            register

        Manual register ->
            register


listenRegisterToString : ListenRegister -> String
listenRegisterToString listenRegister_ =
    case listenRegister_ of
        Auto _ ->
            "Auto"

        Manual register ->
            Register.toString register


type Responsiveness
    = Sensitive
    | Smooth


responsivenessToString : Responsiveness -> String
responsivenessToString responsiveness =
    case responsiveness of
        Sensitive ->
            "Sensitive"

        Smooth ->
            "Smooth"


type PitchFeedback
    = Cents
    | Hz
    | Moria


pitchFeedbackToString : PitchFeedback -> String
pitchFeedbackToString pitchFeedback =
    case pitchFeedback of
        Cents ->
            "Cents"

        Hz ->
            "Hz"

        Moria ->
            "Moria"


setAutoListenRegister : ModeSettings -> AudioSettings -> Frequency -> AudioSettings
setAutoListenRegister modeSettings audioSettings detectedFrequency =
    case audioSettings.listenRegister of
        Auto Bass ->
            let
                detectedPitchIsMeaningfullyHigher =
                    Degree.step modeSettings.rangeEnd 1
                        |> Maybe.Extra.unwrap False
                            (\degree ->
                                PitchPosition.pitchPosition modeSettings.scale degree Nothing
                                    |> Frequency.frequency audioSettings.pitchStandard Bass
                                    |> Frequency.compare detectedFrequency
                                    |> (==) GT
                            )
            in
            if detectedPitchIsMeaningfullyHigher then
                { audioSettings | listenRegister = Auto Treble }

            else
                audioSettings

        Auto Treble ->
            let
                detectedPitchIsMeaningfullyLower =
                    Degree.step modeSettings.rangeStart -1
                        |> Maybe.Extra.unwrap False
                            (\degree ->
                                PitchPosition.pitchPosition modeSettings.scale degree Nothing
                                    |> Frequency.frequency audioSettings.pitchStandard Treble
                                    |> Frequency.compare detectedFrequency
                                    |> (==) LT
                            )
            in
            if detectedPitchIsMeaningfullyLower then
                { audioSettings | listenRegister = Auto Bass }

            else
                audioSettings

        Manual _ ->
            audioSettings
