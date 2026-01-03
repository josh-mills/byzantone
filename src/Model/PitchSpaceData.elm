module Model.PitchSpaceData exposing
    ( PitchSpaceData, init
    , Display, displayToLayout, isVertical, pitchButtonSize
    , PositionWithinVisibleRange(..), calculateVisibleRange, positionIsVisible
    , PitchPositionContextString, encodePitchPositionContext, decodePitchPositionContext
    , intervalsWithVisibility
    , IsonSelectionIndicator, isCurrentIson, canBeSelectedAsIson
    )

{-| Derived data for view logic. All values in here should be primitives,
singletons, or types that can be ensured to be referentially equivalenent
as a result of model updates.


# Pitch Space Data

@docs PitchSpaceData, init


# Display

@docs Display, displayToLayout, isVertical, pitchButtonSize


# Visibility

@docs PositionWithinVisibleRange, calculateVisibleRange, positionIsVisible


# Pitch Positions

@docs PitchPositionContextString, encodePitchPositionContext, decodePitchPositionContext


# Intervals

@docs intervalsWithVisibility


# Ison

@docs IsonSelectionIndicator, isCurrentIson, canBeSelectedAsIson

-}

import Basics.Extra exposing (flip)
import Byzantine.Degree as Degree exposing (Degree)
import Byzantine.Pitch as Pitch exposing (Interval, Pitch)
import Maybe.Extra
import Model.DegreeDataDict as DegreeDataDict exposing (DegreeDataDict)
import Model.LayoutData as LayoutData exposing (Layout(..), LayoutData)
import Model.ModeSettings exposing (ModeSettings)
import Model.PitchState as PitchState exposing (PitchState)
import Movement exposing (Movement)


{-| Derived data for rendering pitch space. Everything in this is derived from
elements stored elsewhere in the model. The denormalization is so we don't need
to re-calculate infrequently changing values in the view code. Data in this
record should be singletons or primitives to support lazy rendering.

Elements include:

  - **`display`** – a `Display` value that encapsulates both the layout and the
    pitch button size, based on the layout selection and/or viewport size for
    auto.

  - **`isonIndicators`** – a `DegreeDataDict IsonSelectionIndicator`, indicating
    whether or not the degree is or can be selected as the ison.

  - **`pitches`** – a `DegreedataDict Pitch`, indicating the specific pitch to
    display in the pitch column.

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
    { display : Display
    , isonIndicators : DegreeDataDict IsonSelectionIndicator
    , pitches : DegreeDataDict Pitch
    , pitchPositions : DegreeDataDict Int
    , pitchVisibility : DegreeDataDict PositionWithinVisibleRange
    , scalingFactor : Float
    , visibleRangeIndexes :
        { startDegreeIndex : Int
        , endDegreeIndex : Int
        }
    , visibleRangeStartPosition : Int
    , visibleRangeEndPosition : Int
    }


{-| Clean reset of data. If we wind up keeping anything more complex than
primitives or singletons in here, we will need to construct targeted updates
that preserve referential equality for values that don't change. Targeted
updates wouldn't be a bad idea for performance, anyway.
-}
init : LayoutData -> ModeSettings -> PitchState -> PitchSpaceData
init layoutData modeSettings pitchState =
    let
        visibleRange =
            calculateVisibleRange modeSettings pitchState

        visibleRangeIndexes =
            { startDegreeIndex = Degree.indexOf (Pitch.unwrapDegree visibleRange.start)
            , endDegreeIndex = Degree.indexOf (Pitch.unwrapDegree visibleRange.end)
            }

        proposedMovementTo =
            Movement.unwrapTargetPitch pitchState.proposedMovement
                |> Maybe.andThen
                    (\pitch ->
                        Pitch.unwrapAccidental pitch
                            |> Maybe.map
                                (\accidental ->
                                    ( Pitch.unwrapDegree pitch, accidental )
                                )
                    )

        pitchWithAccidental degree =
            case proposedMovementTo of
                Just ( movementToDegree, withAccidental ) ->
                    if degree == movementToDegree then
                        Pitch.from modeSettings.scale
                            (Just withAccidental)
                            degree

                    else
                        Pitch.from modeSettings.scale
                            (DegreeDataDict.get degree pitchState.appliedAccidentals)
                            degree

                Nothing ->
                    Pitch.from modeSettings.scale
                        (DegreeDataDict.get degree pitchState.appliedAccidentals)
                        degree
    in
    { display = determineDisplay layoutData
    , isonIndicators = DegreeDataDict.init (isonSelectionIndicator modeSettings pitchState)
    , pitches =
        DegreeDataDict.init
            (\degree ->
                Pitch.from modeSettings.scale
                    (DegreeDataDict.get degree pitchState.appliedAccidentals)
                    degree
            )
    , pitchPositions =
        DegreeDataDict.init
            (pitchWithAccidental
                >> Pitch.pitchPosition modeSettings.scale
            )
    , pitchVisibility = DegreeDataDict.init (visibility visibleRangeIndexes)
    , scalingFactor = 10
    , visibleRangeStartPosition = Pitch.pitchPosition modeSettings.scale visibleRange.start
    , visibleRangeIndexes = visibleRangeIndexes
    , visibleRangeEndPosition = Pitch.pitchPosition modeSettings.scale visibleRange.end
    }
        |> setScalingFactor layoutData


{-| What is the visible range of the pitch space? This expands the default (or
user-set) start and stop positions to include the current pitch. (We may want to
consider additional limits as well.)
-}
calculateVisibleRange : ModeSettings -> PitchState -> { start : Pitch, end : Pitch }
calculateVisibleRange modeSettings pitchState =
    case PitchState.currentPitch modeSettings.scale pitchState of
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



-- DISPLAY


type Display
    = VerticalSmall
    | VerticalLarge
    | HorizontalSmall
    | HorizontalLarge


displayToLayout : Display -> Layout
displayToLayout display =
    case display of
        VerticalSmall ->
            Vertical

        VerticalLarge ->
            Vertical

        HorizontalSmall ->
            Horizontal

        HorizontalLarge ->
            Horizontal


determineDisplay : LayoutData -> Display
determineDisplay layoutData =
    let
        isSmall =
            layoutData.viewport.viewport.width < 640
    in
    case ( LayoutData.layoutFor layoutData, isSmall ) of
        ( Vertical, True ) ->
            VerticalSmall

        ( Vertical, False ) ->
            VerticalLarge

        ( Horizontal, True ) ->
            HorizontalSmall

        ( Horizontal, False ) ->
            HorizontalLarge


isVertical : Display -> Bool
isVertical display =
    case display of
        VerticalSmall ->
            True

        VerticalLarge ->
            True

        HorizontalSmall ->
            False

        HorizontalLarge ->
            False


pitchButtonSize : Display -> Float
pitchButtonSize display =
    case display of
        VerticalSmall ->
            48

        VerticalLarge ->
            64

        HorizontalSmall ->
            48

        HorizontalLarge ->
            64


{-| This feels potentially fragile.

TODO: consider some sort of minimum for the portrait to enable scrolling on
small viewports.

-}
setScalingFactor : LayoutData -> PitchSpaceData -> PitchSpaceData
setScalingFactor layoutData pitchSpaceData =
    let
        visibleRangeInMoria =
            pitchSpaceData.visibleRangeEndPosition - pitchSpaceData.visibleRangeStartPosition
    in
    { pitchSpaceData
        | scalingFactor =
            if isVertical pitchSpaceData.display then
                (layoutData.viewport.viewport.height
                    - max layoutData.pitchSpace.element.y 128
                    - pitchButtonSize pitchSpaceData.display
                )
                    / toFloat visibleRangeInMoria

            else
                (layoutData.pitchSpace.element.width
                    - pitchButtonSize pitchSpaceData.display
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



-- PITCH POSITION


{-| String-encoded representation of the position (in moria) of a pitch,
the pitch one degree below that pitch, and the pitch one degree above that
pitch.
-}
type alias PitchPositionContextString =
    String


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
encodePitchPositionContext : PitchSpaceData -> Degree -> PitchPositionContextString
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
    PitchPositionContextString
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



-- ISON


type IsonSelectionIndicator
    = CurrentIson
    | CanBeSelected_IsCurrent
    | CanBeSelected_NotCurrent
    | NotIson
    | SelectingNotEligible


{-| TODO: we'll need to get modal logic actually built out.
-}
isonSelectionIndicator : ModeSettings -> PitchState -> Degree -> IsonSelectionIndicator
isonSelectionIndicator _ pitchState degree =
    let
        degreeIsEligible =
            Degree.indexOf degree <= 12
    in
    case pitchState.ison of
        PitchState.NoIson ->
            NotIson

        PitchState.SelectingIson (Just currentIson) ->
            if currentIson == degree then
                CanBeSelected_IsCurrent

            else
                CanBeSelected_NotCurrent

        PitchState.SelectingIson Nothing ->
            if degreeIsEligible then
                CanBeSelected_NotCurrent

            else
                SelectingNotEligible

        PitchState.Selected currentIson ->
            if currentIson == degree then
                CurrentIson

            else
                NotIson


isCurrentIson : IsonSelectionIndicator -> Bool
isCurrentIson indicator =
    case indicator of
        CurrentIson ->
            True

        CanBeSelected_IsCurrent ->
            True

        _ ->
            False


canBeSelectedAsIson : IsonSelectionIndicator -> Bool
canBeSelectedAsIson indicator =
    case indicator of
        CanBeSelected_IsCurrent ->
            True

        CanBeSelected_NotCurrent ->
            True

        _ ->
            False



-- INTERVALS


intervalsWithVisibility : PitchSpaceData -> Movement -> List ( Interval, PositionWithinVisibleRange )
intervalsWithVisibility pitchSpaceData movement =
    let
        maybeOverrideMovementTarget =
            Movement.unwrapTargetPitch movement
                |> Maybe.Extra.unwrap identity
                    (\targetPitch ->
                        let
                            targetDegree =
                                Pitch.unwrapDegree targetPitch
                        in
                        \pitch ->
                            if Pitch.unwrapDegree pitch == targetDegree then
                                targetPitch

                            else
                                pitch
                    )
    in
    Degree.gamutList
        |> List.map
            (\degree ->
                ( DegreeDataDict.get degree pitchSpaceData.pitches
                    |> maybeOverrideMovementTarget
                , DegreeDataDict.get degree pitchSpaceData.pitchPositions
                )
            )
        |> intervalsHelper pitchSpaceData


intervalsHelper :
    PitchSpaceData
    -> List ( Pitch, Int )
    -> List ( Interval, PositionWithinVisibleRange )
intervalsHelper pitchSpaceData pitchesWithPositions =
    case pitchesWithPositions of
        a :: b :: rest ->
            getIntervalWithVisibility pitchSpaceData a b
                :: intervalsHelper pitchSpaceData (b :: rest)

        _ ->
            []


getIntervalWithVisibility :
    PitchSpaceData
    -> ( Pitch, Int )
    -> ( Pitch, Int )
    -> ( Interval, PositionWithinVisibleRange )
getIntervalWithVisibility { visibleRangeIndexes } ( fromPitch, fromPitchPosition ) ( toPitch, toPitchPosition ) =
    let
        fromPitchDegreeIndex =
            Pitch.unwrapDegree fromPitch |> Degree.indexOf

        toPitchDegreeIndex =
            Pitch.unwrapDegree toPitch |> Degree.indexOf

        visibleRangeStartIndex =
            visibleRangeIndexes.startDegreeIndex

        visibleRangeEndDegreeIndex =
            visibleRangeIndexes.endDegreeIndex
    in
    ( { from = fromPitch
      , to = toPitch
      , moria = toPitchPosition - fromPitchPosition
      }
    , if toPitchDegreeIndex <= visibleRangeStartIndex then
        Below

      else if fromPitchDegreeIndex >= visibleRangeEndDegreeIndex then
        Above

      else if visibleRangeStartIndex == fromPitchDegreeIndex then
        LowerBoundary

      else if visibleRangeEndDegreeIndex == toPitchDegreeIndex then
        UpperBoundary

      else
        Within
    )



-- PITCH BUTTON CLICKS
{--| Okay, new strategy. Rather than passing in the information needed to
determine the specific Msg to be triggered by clicking on a pitch button,
instead, just pass in whether or not the button can be clicked, or whether it is
highlighted (which also means it can be clicked). The determination of what
updates to the model are needed should happen entirely within the update
function.

Implement with future work.
-}
-- type ClickEligibility
--     = Clickable
--     | Disabled
--     | Highlighted
