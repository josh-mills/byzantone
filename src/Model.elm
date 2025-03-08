module Model exposing
    ( Model, initialModel
    , LayoutSelection(..), Layout(..), layoutFor, layoutString
    , Modal(..), modalOpen, modalToString
    )

{-|


# Model

@docs Model, initialModel


# Layout

@docs LayoutSelection, Layout, layoutFor, layoutString


# Modal

@docs Modal, modalOpen, modalToString

-}

import AudioSettings exposing (AudioSettings)
import Browser.Dom as Dom
import Byzantine.Degree exposing (Degree)
import Byzantine.Scale exposing (Scale(..))
import Movement exposing (Movement(..))


type alias Model =
    { audioSettings : AudioSettings
    , currentPitch : Maybe Degree
    , layoutSelection : LayoutSelection
    , menuOpen : Bool
    , modal : Modal
    , proposedMovement : Movement
    , scale : Scale
    , showSpacing : Bool
    , viewport : Dom.Viewport
    }


initialModel : Model
initialModel =
    { audioSettings = AudioSettings.defaultAudioSettings
    , currentPitch = Nothing
    , layoutSelection = Auto
    , menuOpen = False
    , modal = NoModal
    , proposedMovement = None
    , scale = Diatonic
    , showSpacing = False
    , viewport =
        { scene = { width = 0, height = 0 }
        , viewport = { x = 0, y = 0, width = 0, height = 0 }
        }
    }



-- LAYOUT


type LayoutSelection
    = Auto
    | Manual Layout


type Layout
    = Portrait
    | Landscape


{-| The specifics here will need work. Portrait should be the default, and Auto
should only kick into landscape if the height is sufficiently small, or perhaps
if the ratio is beneath some threshold.
-}
layoutFor : Model -> Layout
layoutFor { layoutSelection, viewport } =
    case layoutSelection of
        Auto ->
            -- TODO: implement.
            Portrait

        Manual layout_ ->
            layout_


layoutString : LayoutSelection -> String
layoutString layoutSelection =
    case layoutSelection of
        Auto ->
            "Auto"

        Manual Portrait ->
            "Portrait"

        Manual Landscape ->
            "Landscape"



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
