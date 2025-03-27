module Model exposing
    ( Model, initialModel
    , PitchState, initialPitchState
    , Modal(..), modalOpen, modalToString
    )

{-|


# Model

@docs Model, initialModel


# Musical State and Settings

@docs PitchState, initialPitchState


# UI State


## Modal

@docs Modal, modalOpen, modalToString

-}

import Byzantine.Degree exposing (Degree(..))
import Byzantine.Scale exposing (Scale(..))
import Model.AudioSettings as AudioSettings exposing (AudioSettings)
import Model.LayoutData as LayoutData exposing (LayoutData)
import Model.ModeSettings as ModeSettings exposing (ModeSettings)
import Movement exposing (Movement(..))


type alias Model =
    { audioSettings : AudioSettings
    , layoutData : LayoutData
    , menuOpen : Bool
    , modal : Modal
    , modeSettings : ModeSettings
    , pitchState : PitchState
    }


initialModel : Model
initialModel =
    { audioSettings = AudioSettings.defaultAudioSettings
    , layoutData = LayoutData.initialLayoutData
    , menuOpen = False
    , modal = NoModal
    , modeSettings = ModeSettings.initialModeSettings
    , pitchState = initialPitchState
    }



-- PITCH STATE


type alias PitchState =
    { currentPitch : Maybe Degree
    , proposedMovement : Movement
    }


initialPitchState : PitchState
initialPitchState =
    { currentPitch = Nothing
    , proposedMovement = None
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
