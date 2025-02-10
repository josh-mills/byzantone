module Model exposing (AudioSettings, Modal(..), Model, Register(..), adjustPitch, initialModel, modalOpen, modalToString)

import Browser.Dom as Dom
import Byzantine.Degree exposing (Degree)
import Byzantine.Scale exposing (Scale(..))
import Movement exposing (Movement(..))


type alias Model =
    { audioSettings : AudioSettings
    , scale : Scale
    , showSpacing : Bool
    , modal : Modal
    , menuOpen : Bool
    , currentPitch : Maybe Degree
    , proposedMovement : Movement
    , viewport : Dom.Viewport
    }


type alias AudioSettings =
    { gain : Float
    , register : Register
    }


type Register
    = Treble
    | Bass


adjustPitch : Register -> Float -> Float
adjustPitch register pitch =
    case register of
        Treble ->
            pitch

        Bass ->
            pitch / 2


initialModel : Model
initialModel =
    { audioSettings =
        { gain = 0.3
        , register = Treble
        }
    , scale = Diatonic
    , showSpacing = False
    , modal = NoModal
    , menuOpen = False
    , currentPitch = Nothing
    , proposedMovement = None
    , viewport =
        { scene = { width = 0, height = 0 }
        , viewport = { x = 0, y = 0, width = 0, height = 0 }
        }
    }



-- MODAL


type Modal
    = NoModal
    | AboutModal
    | SettingsModal


modalToString : Modal -> String
modalToString modal =
    case modal of
        NoModal ->
            ""

        AboutModal ->
            "About"

        SettingsModal ->
            "Settings"


modalOpen : Modal -> Bool
modalOpen modal =
    case modal of
        NoModal ->
            False

        _ ->
            True
