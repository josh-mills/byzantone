module Model.PitchSpaceData exposing (PitchSpaceData, PositionWithinVisibleRange(..), calculateVisibleRange, decodePitchPositionContext, encodePitchPositionContext, init, positionIsVisible)

import Basics.Extra exposing (flip)
import Byzantine.Degree as Degree exposing (Degree)
import Byzantine.Pitch as Pitch exposing (Pitch)
import Maybe.Extra
import Model.DegreeDataDict as DegreeDataDict exposing (DegreeDataDict)
import Model.LayoutData as LayoutData exposing (Layout(..), LayoutData)
import Model.ModeSettings exposing (ModeSettings)
import Model.PitchState exposing (PitchState)
import Movement


{-| Derived data for rendering pitch space. Everything in this is derived from
elements stored elsewhere in the model. The denormalization is so we don't need
to re-calculate infrequently changing values in the view code. Data in this
record should be singletons or primitives to support lazy rendering.

Elements include:

  - **`layout`** – the `Layout` based on the layout selection and/or viewport
    size for auto
  - **`pitchButtonSize`** – a `Float`, representing the size (in pixels) of the
    pitch button based on the viewport.
  - **`pitchPositions`** – a `DegreeDataDict Int`, representing the pitch
    position (in moria) of each degree with respect to the given scale, current
    pitch, and proposed movement with proposed accidental applied as
    appropriate.
  - **`pitchVisibility`** – a `DegreeDataDict PositionWithinVisibleRange`,
    indicating the visibility of each degree in the current pitch space.
  - **`scalingFactor`** – a `Float`, representing the scaling factor for
    translating pitch positions and interval sizes from moria into pixels.
  - **`visibleRangeStart`** – `Int` representing the pitch position (in moria)
    of the bottom of the visible range, which may expand the default or user-set
    position to include the current pitch.
  - **`visibleRangeEnd`** – same idea, but for the top end of the range.

Other elements we'll want:

  - data for each step interval (each one a separate record, which should
    eliminate the need for the to/from pitches), including:
      - moria (int)
  - data for each degree, including:
      - full pitch, including accidental as appropriate (encoded as a string)

-}
type alias PitchSpaceData =
    { layout : Layout
    , pitchButtonSize : Float
    , pitchPositions : DegreeDataDict Int
    , pitchVisibility : DegreeDataDict PositionWithinVisibleRange
    , scalingFactor : Float
    , visibleRangeStart : Int
    , visibleRangeEnd : Int
    }


init : LayoutData -> ModeSettings -> PitchState -> PitchSpaceData
init layoutData modeSettings pitchState =
    let
        visibleRange =
            calculateVisibleRange modeSettings pitchState

        visibleRangeIndexes =
            { startDegreeIndex = Degree.indexOf (Pitch.unwrapDegree visibleRange.start)
            , endDegreeIndex = Degree.indexOf (Pitch.unwrapDegree visibleRange.end)
            }
    in
    { layout = LayoutData.layoutFor layoutData
    , pitchButtonSize = calculatePitchButtonSize layoutData
    , pitchPositions =
        DegreeDataDict.init
            (Pitch.wrapDegree
                pitchState.currentPitch
                (Movement.unwrapTargetPitch pitchState.proposedMovement)
                >> Pitch.pitchPosition modeSettings.scale
            )
    , pitchVisibility = DegreeDataDict.init (visibility visibleRangeIndexes)
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


visibility : { startDegreeIndex : Int, endDegreeIndex : Int } -> Degree -> PositionWithinVisibleRange
visibility { startDegreeIndex, endDegreeIndex } degree =
    let
        degreeIndex =
            Degree.indexOf degree
    in
    if degreeIndex < startDegreeIndex then
        Below

    else if degreeIndex > endDegreeIndex then
        Above

    else if degreeIndex == startDegreeIndex then
        LowerBoundary

    else if degreeIndex == endDegreeIndex then
        UpperBoundary

    else
        Within


{-| Encodes the pitch positions of a degree and its adjacent degrees into a
string format. Takes a `PitchSpaceData` record and a `Degree` and returns a
pipe-separated string containing three values:

1.  The pitch position of the degree one step below the given degree
2.  The pitch position of the given degree
3.  The pitch position of the degree one step above the given degree

If any degree's pitch position is not available in the `pitchPositions`
dictionary, its value in the string will be "\_".

Example: "72|84|96" would represent a degree with position 84 (Di), where the
degree below it is at position 72 and the degree above it is at position 96.

Example: "136|144|_" would represent a degree at the upper bound of the diatonic
scale with position 144, where the degree below it is at position 136 and there
is no degree above it (represented by "_").

-}
encodePitchPositionContext : PitchSpaceData -> Degree -> String
encodePitchPositionContext { pitchPositions } degree =
    let
        getAndEncode =
            Maybe.Extra.unwrap "_"
                (flip DegreeDataDict.get pitchPositions >> String.fromInt)
    in
    [ getAndEncode (Degree.step degree -1)
    , getAndEncode (Just degree)
    , getAndEncode (Degree.step degree 1)
    ]
        |> String.join "|"


{-| Decodes a string created by `encodePitchPositionContext` into a record with
pitch positions for the current degree and adjacent degrees.

The string format is "belowPosition|currentPosition|abovePosition", where each
position is either an integer or "\_" for missing positions.

Example: "72|84|96" decodes to:

  - pitchPosition = 84
  - pitchPositionBelow = Just 72
  - pitchPositionAbove = Just 96

Example: "136|144|\_" decodes to:

  - pitchPosition = 144
  - pitchPositionBelow = Just 136
  - pitchPositionAbove = Nothing

The function returns a Result type, with an error message if parsing fails.

-}
decodePitchPositionContext :
    String
    ->
        Result
            String
            { pitchPosition : Int
            , pitchPositionAbove : Maybe Int
            , pitchPositionBelow : Maybe Int
            }
decodePitchPositionContext string =
    let
        parts =
            String.split "|" string

        parsePosition pos =
            if pos == "_" then
                Ok Nothing

            else
                String.toInt pos
                    |> Maybe.map Ok
                    |> Maybe.withDefault (Err <| "Invalid pitch position: " ++ pos)
                    |> Result.map Just
    in
    case parts of
        [ below, current, above ] ->
            case String.toInt current of
                Just position ->
                    Result.map2
                        (\belowPos abovePos ->
                            { pitchPosition = position
                            , pitchPositionBelow = belowPos
                            , pitchPositionAbove = abovePos
                            }
                        )
                        (parsePosition below)
                        (parsePosition above)

                Nothing ->
                    Err <| "Invalid current pitch position: " ++ current

        _ ->
            Err <| "Invalid pitch position context format: " ++ string
