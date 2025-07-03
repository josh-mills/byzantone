module Model.PitchSpaceData exposing (PitchSpaceData, init)

import Model.LayoutData as LayoutData exposing (Layout, LayoutData)
import Model.ModeSettings exposing (ModeSettings)
import Model.PitchState exposing (PitchState)


{-| Derived data for rendering pitch space. Everything in this is derived from
elements stored elsewhere in the model. The denormalization is so we don't need
to re-calculate infrequently changing values in the view code. Data in this
record should be singletons or primitives to support lazy rendering.

Other elements we'll want:

  - scaling factor (float)
  - pitch button size (float)
  - data for each step interval (each one a separate record, which should
    eliminate the need for the to/from pitches), including:
      - position within visible range (singleton)
      - moria (int)
  - data for each degree, including:
      - full pitch, including accidental as appropriate (encoded as a string)
      - pitch position (int)
      - position within visible range (singleton)

-}
type alias PitchSpaceData =
    { layout : Layout
    }


init : LayoutData -> ModeSettings -> PitchState -> PitchSpaceData
init layoutData _ _ =
    { layout = LayoutData.layoutFor layoutData
    }
