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


{-| TODO: separate playing and listening registers as two independent fields.
Types can remain the same, though.
-}
type alias AudioSettings =
    { gain : Float
    , mode : Mode
    , pitchStandard : PitchStandard
    , register : Register
    , responsiveness : Responsiveness
    }


defaultAudioSettings : AudioSettings
defaultAudioSettings =
    { gain = 0.3
    , mode = Play
    , pitchStandard = Ni256
    , register = Treble
    , responsiveness = Sensitive
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
