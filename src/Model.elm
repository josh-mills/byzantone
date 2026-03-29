module Model exposing
    ( Model, init
    , Remote
    , Modal(..), modalOpen, modalToString
    )

{-|


# Model

@docs Model, init


## Remote

@docs Remote


## Modal

@docs Modal, modalOpen, modalToString

-}

import Byzantine.DetectedPitch exposing (DetectedPitch)
import Http
import Model.AudioSettings as AudioSettings exposing (AudioSettings)
import Model.ControlsMenu as ControlsMenu exposing (OpenControlMenus)
import Model.DeviceInfo exposing (DeviceInfo)
import Model.LayoutData as LayoutData exposing (LayoutData)
import Model.ModeSettings as ModeSettings exposing (ModeSettings)
import Model.PitchSpaceData as PitchSpaceData exposing (PitchSpaceData)
import Model.PitchState as PitchState exposing (PitchState)
import Remote.AboutCopy exposing (AboutCopy)
import Remote.Changelog exposing (Changelog)
import RemoteData exposing (RemoteData)
import Time


type alias Model =
    { audioSettings : AudioSettings
    , remote : Remote
    , detectedPitch : Maybe { detectedPitch : DetectedPitch, timestamp : Time.Posix }
    , deviceInfo : DeviceInfo
    , headerIsOpen : Bool
    , layoutData : LayoutData
    , menuOpen : Bool
    , modal : Modal
    , modeSettings : ModeSettings
    , openControlMenus : OpenControlMenus
    , pitchSpaceData : PitchSpaceData
    , pitchState : PitchState
    }


type alias Remote =
    { changelog : RemoteData Http.Error Changelog
    , aboutCopy : RemoteData Http.Error AboutCopy
    }


init : DeviceInfo -> { width : Float, height : Float } -> Model
init deviceInfo viewportDimensions =
    let
        layoutData =
            LayoutData.init viewportDimensions
    in
    { audioSettings = AudioSettings.defaultAudioSettings
    , remote =
        { changelog = RemoteData.NotAsked
        , aboutCopy = RemoteData.NotAsked
        }
    , detectedPitch = Nothing
    , deviceInfo = deviceInfo
    , headerIsOpen = True
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
