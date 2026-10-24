module Model.ModeSettings exposing (ModeSettings, initialModeSettings, scale)

{-| User-controlled settings for the mode. Minimalist at present, but this will
expand as the capability to select actual modes (beyond just a base scale) is
built out.
-}

import Byzantine.Degree exposing (Degree(..))
import Byzantine.Mode as Mode
import Byzantine.Scale exposing (Scale(..))
import ModeBuilder exposing (Selection(..))


type alias ModeSettings =
    { rangeStart : Degree
    , rangeEnd : Degree
    , selection : Maybe Selection
    }


initialModeSettings : ModeSettings
initialModeSettings =
    { rangeStart = Ni
    , rangeEnd = Pa_
    , selection = Nothing
    }


{-| The scale in effect for the current selection. Defaults to `Diatonic` when
nothing has been selected.
-}
scale : ModeSettings -> Scale
scale modeSettings =
    case modeSettings.selection of
        Just (ByScale selectedScale) ->
            selectedScale

        Just (ByMode mode) ->
            (Mode.data mode).scale

        Nothing ->
            Diatonic
