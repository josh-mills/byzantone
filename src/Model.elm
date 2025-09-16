module Model exposing
    ( Model, initialModel
    , Modal(..), modalOpen, modalToString
    )

{-|


# Model

@docs Model, initialModel


# Controls Menus

@docs ControlMenu


## Modal

@docs Modal, modalOpen, modalToString

-}

import Byzantine.Degree exposing (Degree(..))
import Byzantine.Frequency exposing (Frequency)
import Byzantine.Scale exposing (Scale(..))
import ControlsMenu exposing (OpenControlMenus)
import Model.AudioSettings as AudioSettings exposing (AudioSettings)
import Model.LayoutData as LayoutData exposing (LayoutData)
import Model.ModeSettings as ModeSettings exposing (ModeSettings)
import Model.PitchSpaceData as PitchSpaceData exposing (PitchSpaceData)
import Model.PitchState as PitchState exposing (PitchState)
import Movement exposing (Movement(..))


type alias Model =
    { audioSettings : AudioSettings
    , detectedPitch : Maybe Frequency
    , layoutData : LayoutData
    , menuOpen : Bool
    , modal : Modal
    , modeSettings : ModeSettings
    , openControlMenus : OpenControlMenus
    , pitchSpaceData : PitchSpaceData
    , pitchState : PitchState
    }


initialModel : Model
initialModel =
    { audioSettings = AudioSettings.defaultAudioSettings
    , detectedPitch = Nothing
    , layoutData = LayoutData.initialLayoutData
    , menuOpen = False
    , modal = NoModal
    , modeSettings = ModeSettings.initialModeSettings
    , openControlMenus = ControlsMenu.init
    , pitchSpaceData =
        PitchSpaceData.init
            LayoutData.initialLayoutData
            ModeSettings.initialModeSettings
            PitchState.initialPitchState
    , pitchState = PitchState.initialPitchState
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
