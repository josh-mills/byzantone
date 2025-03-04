module Model exposing (Modal(..), Model, initialModel, modalOpen, modalToString)

import AudioSettings exposing (AudioSettings)
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


initialModel : Model
initialModel =
    { audioSettings = AudioSettings.defaultAudioSettings
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
