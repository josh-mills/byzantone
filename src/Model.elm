module Model exposing
    ( Model, initialModel
    , LayoutData, LayoutSelection(..), Layout(..), layoutFor, layoutString
    , Modal(..), modalOpen, modalToString
    )

{-|


# Model

@docs Model, initialModel


# Layout

@docs LayoutData, LayoutSelection, Layout, layoutFor, layoutString


# Modal

@docs Modal, modalOpen, modalToString

-}

import AudioSettings exposing (AudioSettings)
import Browser.Dom as Dom
import Byzantine.Degree exposing (Degree(..))
import Byzantine.Scale exposing (Scale(..))
import Movement exposing (Movement(..))


type alias Model =
    { audioSettings : AudioSettings
    , currentPitch : Maybe Degree
    , layoutData : LayoutData
    , menuOpen : Bool
    , modal : Modal
    , proposedMovement : Movement
    , rangeStart : Degree
    , rangeEnd : Degree
    , scale : Scale
    }


initialModel : Model
initialModel =
    { audioSettings = AudioSettings.defaultAudioSettings
    , currentPitch = Nothing
    , layoutData =
        { layoutSelection = Auto
        , pitchSpace = defaultElement
        , showSpacing = False
        , viewport = defaultViewport
        }
    , menuOpen = False
    , modal = NoModal
    , rangeStart = Ni
    , rangeEnd = Ni_
    , proposedMovement = None
    , scale = Diatonic
    }


{-| Initial hardcoded height 256 prevents negative width settings which
enables a smooth css transition.
-}
defaultViewport : Dom.Viewport
defaultViewport =
    { scene = { width = 0, height = 0 }
    , viewport = { x = 0, y = 0, width = 0, height = 256 }
    }


{-| Initial hardcoded element width of 64 prevents negative width settings which
enables a smooth css transition.
-}
defaultElement : Dom.Element
defaultElement =
    { scene = { width = 0, height = 0 }
    , viewport = { x = 0, y = 0, width = 0, height = 0 }
    , element = { x = 0, y = 0, width = 64, height = 0 }
    }



-- LAYOUT


type alias LayoutData =
    { layoutSelection : LayoutSelection
    , pitchSpace : Dom.Element
    , showSpacing : Bool
    , viewport : Dom.Viewport
    }


type LayoutSelection
    = Auto
    | Manual Layout


type Layout
    = Vertical
    | Horizontal


{-| For Auto layout, Vertical is the default. Only when the screen is relatively
short _and_ significantly wider than it is tall should the layout switch to
Horizontal.
-}
layoutFor : LayoutData -> Layout
layoutFor { layoutSelection, viewport } =
    case layoutSelection of
        Auto ->
            let
                ratio =
                    viewport.viewport.height / viewport.viewport.width
            in
            if viewport.viewport.height < 800 && ratio < 2.2 then
                Horizontal

            else
                Vertical

        Manual layout ->
            layout


layoutString : LayoutSelection -> String
layoutString layoutSelection =
    case layoutSelection of
        Auto ->
            "Auto"

        Manual Vertical ->
            "Vertical"

        Manual Horizontal ->
            "Horizontal"



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
