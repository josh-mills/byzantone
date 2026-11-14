module Model.ModeSettings exposing (ModeSettings, initialModeSettings, mode, pitchFrom, range, relevantAccidentals, scale, signature)

{-| User-controlled settings for the mode. Minimalist at present, but this will
expand as the capability to select actual modes (beyond just a base scale) is
built out.
-}

import Byzantine.Accidental as Accidental exposing (Accidental)
import Byzantine.Degree exposing (Degree(..))
import Byzantine.Mode as Mode exposing (Mode, Range)
import Byzantine.Mode.Signature exposing (Signature)
import Byzantine.Pitch as Pitch exposing (Pitch)
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
    case mode modeSettings of
        Just selectedMode ->
            (Mode.data selectedMode).range

        Nothing ->
            modeSettings.range


{-| The scale in effect for the current selection. Defaults to `Diatonic` when
nothing has been selected.
-}
scale : ModeSettings -> Scale
scale modeSettings =
    case modeSettings.selection of
        Just (ByScale selectedScale) ->
            selectedScale

        Just (ByMode selected) ->
            (Mode.data selected.mode).scale

        Nothing ->
            SoftDiatonic


{-| The selected mode, if a defined mode (rather than only a scale) has been
selected.
-}
mode : ModeSettings -> Maybe Mode
mode modeSettings =
    case modeSettings.selection of
        Just (ByMode selected) ->
            Just selected.mode

        _ ->
            Nothing


{-| The signature the user selected, if a defined mode (rather than only a scale)
has been selected.
-}
signature : ModeSettings -> Maybe Signature
signature modeSettings =
    case modeSettings.selection of
        Just (ByMode selected) ->
            Just selected.signature

        _ ->
            Nothing


{-| The accidentals that are relevant to the current selection, sorted by moria
adjustment. A selected mode supplies its own (see `Mode.relevantAccidentals`);
otherwise every accidental is relevant.
-}
relevantAccidentals : ModeSettings -> List Accidental
relevantAccidentals modeSettings =
    case mode modeSettings of
        Just selectedMode ->
            Mode.relevantAccidentals selectedMode

        Nothing ->
            Accidental.all


{-| Construct a pitch for a degree with an optional accidental. If a defined mode
is selected, the mode determines whether the accidental is valid for the degree
(see `Mode.pitchFrom`); otherwise the scale in effect does (see `Pitch.from`).
-}
pitchFrom : ModeSettings -> Maybe Accidental -> Degree -> Pitch
pitchFrom modeSettings =
    case mode modeSettings of
        Just selectedMode ->
            Mode.pitchFrom selectedMode

        Nothing ->
            Pitch.from (scale modeSettings)
