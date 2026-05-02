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
import Byzantine.Mode exposing (Mode)
import Date exposing (Date)
import Http
import ModeBuilder
import Model.AudioSettings as AudioSettings exposing (AudioSettings)
import Model.CalendarInfo as CalendarInfo exposing (CalendarInfo)
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
    , calendar : CalendarInfo
    , detectedPitch : Maybe { detectedPitch : DetectedPitch, timestamp : Time.Posix }
    , deviceInfo : DeviceInfo
    , headerIsOpen : Bool
    , layoutData : LayoutData
    , menuOpen : Bool
    , modal : Modal
    , modeBuilder : ModeBuilder.Model
    , modeSettings : ModeSettings
    , openControlMenus : OpenControlMenus
    , pitchSpaceData : PitchSpaceData
    , pitchState : PitchState
    , remote : Remote
    }


type alias Remote =
    { changelog : RemoteData Http.Error Changelog
    , aboutCopy : RemoteData Http.Error AboutCopy
    }


init : DeviceInfo -> { width : Float, height : Float } -> Maybe Date -> Model
init deviceInfo viewportDimensions currentDate =
    let
        layoutData =
            LayoutData.init viewportDimensions
    in
    { audioSettings = AudioSettings.defaultAudioSettings
    , calendar = CalendarInfo.init currentDate
    , detectedPitch = Nothing
    , deviceInfo = deviceInfo
    , headerIsOpen = True
    , layoutData = layoutData
    , menuOpen = False
    , modal = NoModal
    , modeBuilder = ModeBuilder.init
    , modeSettings = ModeSettings.initialModeSettings
    , openControlMenus = ControlsMenu.init
    , pitchSpaceData =
        PitchSpaceData.init
            layoutData
            ModeSettings.initialModeSettings
            PitchState.initialPitchState
    , pitchState = PitchState.initialPitchState
    , remote =
        { changelog = RemoteData.NotAsked
        , aboutCopy = RemoteData.NotAsked
        }
    }



-- MODAL


type Modal
    = NoModal
    | AboutModal
    | ModeModal (Maybe Mode)
    | SettingsModal
    | ReleasesModal Bool


modalToString : Modal -> String
modalToString modal =
    case modal of
        NoModal ->
            ""

        AboutModal ->
            "About"

        ModeModal _ ->
            "Mode Info"

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
