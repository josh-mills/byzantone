module Model.ModeSettings exposing (ModeSettings, initialModeSettings)

{-| User-controlled settings for the mode. Minimalist at present, but this will
expand as the capability to select actual modes (beyond just a base scale) is
built out.
-}

import Byzantine.Degree exposing (Degree(..))
import Byzantine.Scale exposing (Scale(..))


type alias ModeSettings =
    { rangeStart : Degree
    , rangeEnd : Degree
    , scale : Scale
    }


initialModeSettings : ModeSettings
initialModeSettings =
    { rangeStart = Ni
    , rangeEnd = Ni_
    , scale = Diatonic
    }
