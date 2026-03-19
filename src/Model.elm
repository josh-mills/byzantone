module Model exposing
    ( Model, init
    , Modal(..), modalOpen, modalToString
    )

{-|


# Model

@docs Model, init


## Modal

@docs Modal, modalOpen, modalToString

-}

import Byzantine.DetectedPitch exposing (DetectedPitch)
import Http
import Model.AudioSettings as AudioSettings exposing (AudioSettings)
import Model.Changelog exposing (Changelog)
import Model.ControlsMenu as ControlsMenu exposing (OpenControlMenus)
import Model.Copy exposing (Copy)
import Model.DeviceInfo exposing (DeviceInfo)
import Model.LayoutData as LayoutData exposing (LayoutData)
import Model.ModeSettings as ModeSettings exposing (ModeSettings)
import Model.PitchSpaceData as PitchSpaceData exposing (PitchSpaceData)
import Model.PitchState as PitchState exposing (PitchState)
import RemoteData exposing (RemoteData)
import Time


type alias Model =
    { audioSettings : AudioSettings
    , changelog : RemoteData Http.Error Changelog
    , copy : RemoteData Http.Error Copy
    , detectedPitch : Maybe { detectedPitch : DetectedPitch, timestamp : Time.Posix }
    , deviceInfo : DeviceInfo
    , headerCollapsed : Bool
    , layoutData : LayoutData
    , menuOpen : Bool
    , modal : Modal
    , modeSettings : ModeSettings
    , openControlMenus : OpenControlMenus
    , pitchSpaceData : PitchSpaceData
    , pitchState : PitchState
    }


init : DeviceInfo -> { width : Float, height : Float } -> Model
init deviceInfo viewportDimensions =
    let
        layoutData =
            LayoutData.init viewportDimensions
    in
    { audioSettings = AudioSettings.defaultAudioSettings
    , changelog = RemoteData.NotAsked
    , copy = RemoteData.NotAsked
    , detectedPitch = Nothing
    , deviceInfo = deviceInfo
    , headerCollapsed = False
    , layoutData = layoutData
    , menuOpen = False
    , modal = NoModal
    , modeSettings = ModeSettings.initialModeSettings
    , openControlMenus = ControlsMenu.init
    , pitchSpaceData =
        PitchSpaceData.init
            layoutData
            ModeSettings.initialModeSettings
            PitchState.initialPitchState
    , pitchState = PitchState.initialPitchState
    }



-- MODAL


type Modal
    = NoModal
    | AboutModal
    | SettingsModal
    | ReleasesModal Bool


modalToString : Modal -> String
modalToString modal =
    case modal of
        NoModal ->
            ""

        AboutModal ->
            "About"

        SettingsModal ->
            "Settings"

        ReleasesModal _ ->
            "Release Notes"


modalOpen : Modal -> Bool
modalOpen modal =
    case modal of
        NoModal ->
            False

        _ ->
            True
