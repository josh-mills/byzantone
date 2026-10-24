module Model.ModeSettings exposing (ModeSettings, initialModeSettings, range, scale)

{-| User-controlled settings for the mode. Minimalist at present, but this will
expand as the capability to select actual modes (beyond just a base scale) is
built out.
-}

import Byzantine.Degree exposing (Degree(..))
import Byzantine.Mode as Mode exposing (Range)
import Byzantine.Scale exposing (Scale(..))
import ModeBuilder exposing (Selection(..))


type alias ModeSettings =
    { range : Range
    , selection : Maybe Selection
    }


initialModeSettings : ModeSettings
initialModeSettings =
    { range = { start = Ni, end = Pa_ }
    , selection = Nothing
    }


{-| The range in effect. A selected mode supplies its own range; otherwise the
range stored in the settings is used.
-}
range : ModeSettings -> Range
range modeSettings =
    case modeSettings.selection of
        Just (ByMode mode) ->
            (Mode.data mode).range

        _ ->
            modeSettings.range


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
