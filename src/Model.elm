module Model exposing
    ( Model, initialModel
    , Modal(..), modalOpen, modalToString
    )

{-|


# Model

@docs Model, initialModel


## Modal

@docs Modal, modalOpen, modalToString

-}

import Byzantine.Degree exposing (Degree(..))
import Byzantine.Scale exposing (Scale(..))
import Model.AudioSettings as AudioSettings exposing (AudioSettings)
import Model.LayoutData as LayoutData exposing (LayoutData)
import Model.ModeSettings as ModeSettings exposing (ModeSettings)
import Model.PitchState as PitchState exposing (PitchState)
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
