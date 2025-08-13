module Model.AudioSettings exposing
    ( AudioSettings, defaultAudioSettings
    , Mode(..), audioModeToString, modes
    )

{-|

@docs AudioSettings, defaultAudioSettings
@docs Mode, audioModeToString, modes

-}

import Byzantine.Degree exposing (Degree(..))
import Byzantine.Pitch exposing (PitchStandard(..), Register(..))


type alias AudioSettings =
    { gain : Float
    , mode : Mode
    , pitchStandard : PitchStandard
    , register : Register
    }


defaultAudioSettings : AudioSettings
defaultAudioSettings =
    { gain = 0.3
    , mode = Play
    , pitchStandard = Ni256
    , register = Treble
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
