module Model.AudioSettings exposing
    ( AudioSettings, defaultAudioSettings
    , Mode(..), audioModeToString, modes
    , Responsiveness(..), responsivenessToString
    )

{-|

@docs AudioSettings, defaultAudioSettings
@docs Mode, audioModeToString, modes
@docs Responsiveness, responsivenessToString

-}

import Byzantine.Degree exposing (Degree(..))
import Byzantine.Pitch exposing (PitchStandard(..), Register(..))


type alias AudioSettings =
    { gain : Float
    , mode : Mode
    , pitchStandard : PitchStandard
    , playbackRegister : Register
    , listenRegister : Register
    , responsiveness : Responsiveness
    }


defaultAudioSettings : AudioSettings
defaultAudioSettings =
    { gain = 0.3
    , mode = Play
    , pitchStandard = Ni256
    , playbackRegister = Treble
    , listenRegister = Bass
    , responsiveness = Smooth
    }


type Mode
    = Play
    | Listen


modes : List Mode
modes =
    [ Play, Listen ]


audioModeToString : Mode -> String
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
