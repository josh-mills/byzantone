module Model exposing (AudioSettings, Model, initialModel)

import Browser.Dom as Dom
import Byzantine.Degree exposing (Degree)
import Byzantine.Scale exposing (Scale(..))
import Movement exposing (Movement(..))


type alias Model =
    { audioSettings : AudioSettings
    , scale : Scale
    , showSpacing : Bool
    , currentPitch : Maybe Degree
    , proposedMovement : Movement
    , viewport : Dom.Viewport
    }


type alias AudioSettings =
    { gain : Float }


initialModel : Model
initialModel =
    { audioSettings =
        { gain = 0.3 }
    , scale = Diatonic
    , showSpacing = False
    , currentPitch = Nothing
    , proposedMovement = None
    , viewport =
        { scene = { width = 0, height = 0 }
        , viewport = { x = 0, y = 0, width = 0, height = 0 }
        }
    }
