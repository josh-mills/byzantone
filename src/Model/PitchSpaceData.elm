module Model.PitchSpaceData exposing (PitchSpaceData, PositionWithinVisibleRange(..), calculateVisibleRange, init, positionIsVisible)

import Byzantine.Degree as Degree
import Byzantine.Pitch as Pitch exposing (Pitch)
import Model.LayoutData as LayoutData exposing (Layout(..), LayoutData)
import Model.ModeSettings exposing (ModeSettings)
import Model.PitchState exposing (PitchState)


{-| Derived data for rendering pitch space. Everything in this is derived from
elements stored elsewhere in the model. The denormalization is so we don't need
to re-calculate infrequently changing values in the view code. Data in this
record should be singletons or primitives to support lazy rendering.

Elements include:

  - **`layout`** – the `Layout` based on the layout selection and/or viewport
    size for auto
  - **`pitchButtonSize`** – a `Float`, representing the size (in pixels) of the
    pitch button based on the viewport.
  - **`scalingFactor`** – a `Float`, representing the scaling factor for
    translating pitch positions and interval sizes from moria into pixels.
  - **`visibleRangeStart`** – `Int` representing the pitch position (in moria)
    of the bottom of the visible range, which may expand the default or user-set
    position to include the current pitch.
  - **`visibleRangeEnd`** – same idea, but for the top end of the range.

Other elements we'll want:

  - data for each step interval (each one a separate record, which should
    eliminate the need for the to/from pitches), including:
      - position within visible range (singleton)
      - moria (int)
  - data for each degree, including:
      - full pitch, including accidental as appropriate (encoded as a string)
      - pitch position (int)
      - pitch position above (int? maybe int?)
      - pitch position below (int? maybe int?)
      - position within visible range (singleton)

-}
type alias PitchSpaceData =
    { layout : Layout
    , pitchButtonSize : Float
    , scalingFactor : Float
    , visibleRangeStart : Int
    , visibleRangeEnd : Int
    }


init : LayoutData -> ModeSettings -> PitchState -> PitchSpaceData
init layoutData modeSettings pitchState =
    let
        visibleRange =
            calculateVisibleRange modeSettings pitchState
    in
    { layout = LayoutData.layoutFor layoutData
    , pitchButtonSize = calculatePitchButtonSize layoutData
    , scalingFactor = 10
    , visibleRangeStart = Pitch.pitchPosition modeSettings.scale visibleRange.start
    , visibleRangeEnd = Pitch.pitchPosition modeSettings.scale visibleRange.end
    }
        |> setScalingFactor layoutData


calculatePitchButtonSize : LayoutData -> Float
calculatePitchButtonSize layoutData =
    if layoutData.viewport.viewport.width < 640 then
        48

    else
        64


{-| What is the visible range of the pitch space? This expands the default (or
user-set) start and stop positions to include the current pitch. (We may want to
consider additional limits as well.)
-}
calculateVisibleRange : ModeSettings -> PitchState -> { start : Pitch, end : Pitch }
calculateVisibleRange modeSettings pitchState =
    case pitchState.currentPitch of
        Just currentPitch ->
            let
                currentDegree =
                    Pitch.unwrapDegree currentPitch
            in
            { start =
                if Degree.indexOf currentDegree < Degree.indexOf modeSettings.rangeStart then
                    currentPitch

                else
                    Pitch.natural modeSettings.rangeStart
            , end =
                if Degree.indexOf currentDegree > Degree.indexOf modeSettings.rangeEnd then
                    currentPitch

                else
                    Pitch.natural modeSettings.rangeEnd
            }

        Nothing ->
            { start = Pitch.natural modeSettings.rangeStart
            , end = Pitch.natural modeSettings.rangeEnd
            }


{-| This feels potentially fragile.

TODO: consider some sort of minimum for the portrait to enable scrolling on
small viewports.

-}
setScalingFactor : LayoutData -> PitchSpaceData -> PitchSpaceData
setScalingFactor layoutData pitchSpaceData =
    let
        visibleRangeInMoria =
            pitchSpaceData.visibleRangeEnd - pitchSpaceData.visibleRangeStart
    in
    { pitchSpaceData
        | scalingFactor =
            case pitchSpaceData.layout of
                Vertical ->
                    (layoutData.viewport.viewport.height
                        - max layoutData.pitchSpace.element.y 128
                        - pitchSpaceData.pitchButtonSize
                    )
                        / toFloat visibleRangeInMoria

                Horizontal ->
                    (layoutData.pitchSpace.element.width
                        - pitchSpaceData.pitchButtonSize
                    )
                        / toFloat visibleRangeInMoria
    }



-- VISIBILITY


type PositionWithinVisibleRange
    = Below
    | LowerBoundary
    | Within
    | UpperBoundary
    | Above


positionIsVisible : PositionWithinVisibleRange -> Bool
positionIsVisible position =
    case position of
        Below ->
            False

        LowerBoundary ->
            True

        Within ->
            True

        UpperBoundary ->
            True

        Above ->
            False
