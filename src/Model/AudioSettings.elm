module Model.AudioSettings exposing
    ( AudioSettings, defaultAudioSettings
    , AudioMode(..), audioModeToString, modes
    , Responsiveness(..), responsivenessToString
    )

{-|

@docs AudioSettings, defaultAudioSettings
@docs AudioMode, audioModeToString, modes
@docs Responsiveness, responsivenessToString

-}

import Byzantine.Degree exposing (Degree(..))
import Byzantine.Frequency exposing (PitchStandard(..))
import Byzantine.Register exposing (Register(..))


type alias AudioSettings =
    { gain : Float
    , audioMode : AudioMode
    , pitchStandard : PitchStandard
    , playbackRegister : Register
    , listenRegister : Register
    , responsiveness : Responsiveness
    }


defaultAudioSettings : AudioSettings
defaultAudioSettings =
    { gain = 0.3
    , audioMode = Play
    , pitchStandard = Ni256
    , playbackRegister = Treble
    , listenRegister = Bass
    , responsiveness = Smooth
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
