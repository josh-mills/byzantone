module AudioSettings exposing (AudioSettings, defaultAudioSettings)

import Byzantine.Degree exposing (Degree(..))
import Byzantine.Pitch exposing (PitchStandard(..), Register(..))


type alias AudioSettings =
    { gain : Float
    , pitchStandard : PitchStandard
    , register : Register
    }


defaultAudioSettings : AudioSettings
defaultAudioSettings =
    { gain = 0.3
    , pitchStandard = Ni256
    , register = Treble
    }
