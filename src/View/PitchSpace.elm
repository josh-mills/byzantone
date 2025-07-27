module View.PitchSpace exposing (view)

{-| View logic for pitch space (i.e., the intervalic space and positioned pitches)
-}

import Byzantine.Accidental as Accidental
import Byzantine.ByzHtml.Accidental as Accidental
import Byzantine.ByzHtml.Interval as ByzHtmlInterval
import Byzantine.ByzHtml.Martyria as ByzHtmlMartyria
import Byzantine.Degree as Degree exposing (Degree)
import Byzantine.IntervalCharacter as IntervalCharacter
import Byzantine.Martyria as Martyria
import Byzantine.Pitch as Pitch exposing (Interval, Pitch, PitchString)
import Html exposing (Html, button, div, li, span, text)
import Html.Attributes as Attr exposing (class, classList)
import Html.Attributes.Extra as Attr
import Html.Events exposing (onClick, onFocus, onMouseEnter, onMouseLeave)
import Html.Extra exposing (viewIf, viewIfLazy)
import Html.Lazy
import Maybe.Extra as Maybe
import Model.DegreeDataDict as DegreeDataDict
import Model.LayoutData as LayoutData exposing (Layout(..))
import Model.ModeSettings exposing (ModeSettings)
import Model.PitchSpaceData as PitchSpaceData
    exposing
        ( Display
        , IsonSelectionIndicator
        , PitchPositionContextString
        , PitchSpaceData
        , PositionWithinVisibleRange(..)
        , calculateVisibleRange
        )
import Model.PitchState exposing (IsonStatus(..), PitchState)
import Movement exposing (Movement(..))
import Result.Extra
import Round
import Styles
import Update exposing (Msg(..))



-- WRAPPER AND VIEW HELPERS


{-| How we might refactor things to take advantage of lazy rendering? We only
have three arguments, with everything else being derived, and the only dynamic
items really being those from PitchState.

If we are able to split out the pitchState arguments so they are passed in on an
as-needed basis, and if derived values could be passed in as primitives (a
Layout I think should also be fine), I think we should be able to lazify this.

Consider treating the visibleRange values as two ints (moria pitch positions).

-}
view : PitchSpaceData -> ModeSettings -> PitchState -> Html Msg
view pitchSpaceData modeSettings pitchState =
    div
        ([ Attr.id "pitch-space"
         , Styles.transition
         , Attr.attributeIf LayoutData.showSpacing Styles.border
         ]
            ++ (if PitchSpaceData.isVertical pitchSpaceData.display then
                    [ Styles.flexRow, class "my-8" ]

                else
                    [ Styles.flexCol, class "mx-8" ]
               )
        )
        [ viewIntervals pitchSpaceData modeSettings pitchState
        , viewPitches pitchSpaceData modeSettings pitchState
        ]


listAttributes : Display -> List (Html.Attribute Msg)
listAttributes display =
    if PitchSpaceData.isVertical display then
        [ class "flex flex-col-reverse justify-end w-36 mx-4" ]

    else
        [ Styles.flexRowCentered
        , class "h-24 w-full my-4"
        ]


{-| 64px default, 48px below the sm breakpoint.
-}
pitchButtonSizeClass : Html.Attribute msg
pitchButtonSizeClass =
    class "w-12 h-12 sm:w-16 sm:h-16 text-xl sm:text-3xl"



-- INTERVAL COLUMN


{-| The core challenge with this, from a standpoint of lazy rendering, is that
the intervalsWithVisibility list is dynamically calculated. So in the view JS
code, these will be new objects with different references. I'm not sure if
there's a good way to get around that, unless you find a way to encode the
interval as a primitive.
-}
viewIntervals : PitchSpaceData -> ModeSettings -> PitchState -> Html Msg
viewIntervals pitchSpaceData modeSettings pitchState =
    Html.ol (onMouseLeave (SelectProposedMovement None) :: listAttributes pitchSpaceData.display)
        (List.map
            (viewInterval pitchSpaceData.display pitchSpaceData.scalingFactor pitchState)
            (intervalsWithVisibility modeSettings pitchState (calculateVisibleRange modeSettings pitchState))
        )


intervalsWithVisibility :
    ModeSettings
    -> PitchState
    -> { start : Pitch, end : Pitch }
    -> List ( Interval, PositionWithinVisibleRange )
intervalsWithVisibility modeSettings pitchState visibleRange =
    let
        lowerBoundIndex =
            Degree.indexOf (Pitch.unwrapDegree visibleRange.start)

        upperBoundIndex =
            Degree.indexOf (Pitch.unwrapDegree visibleRange.end)

        visibility interval =
            let
                intervalFromIndex =
                    Degree.indexOf (Pitch.unwrapDegree interval.from)

                intervalToIndex =
                    Degree.indexOf (Pitch.unwrapDegree interval.to)
            in
            if intervalToIndex <= lowerBoundIndex then
                Below

            else if intervalFromIndex >= upperBoundIndex then
                Above

            else if lowerBoundIndex == intervalFromIndex then
                LowerBoundary

            else if upperBoundIndex == intervalToIndex then
                UpperBoundary

            else
                Within
    in
    List.map
        (\interval ->
            ( interval
            , visibility interval
            )
        )
        (Pitch.intervals modeSettings.scale
            pitchState.currentPitch
            (Movement.unwrapTargetPitch pitchState.proposedMovement)
        )


viewInterval : Display -> Float -> PitchState -> ( Interval, PositionWithinVisibleRange ) -> Html Msg
viewInterval display scalingFactor pitchState ( interval, position ) =
    let
        -- _ =
        --     Debug.log "in viewInterval" interval
        size =
            case position of
                Above ->
                    0

                Below ->
                    0

                _ ->
                    toFloat interval.moria * scalingFactor

        movement =
            Movement.ofInterval pitchState.currentPitch interval

        moria =
            span [ class "text-gray-600" ]
                [ text (String.fromInt interval.moria)
                , viewIf LayoutData.showSpacing
                    (text <| " (" ++ Round.round 2 size ++ "px)")
                ]

        buttonAttrs =
            [ class "w-full content-center cursor-pointer"
            , classList
                [ ( "bg-slate-200"
                  , shouldHighlightInterval pitchState interval
                  )
                , ( "hover:bg-slate-200", Maybe.isJust pitchState.currentPitch )
                ]
            , onFocus (SelectProposedMovement movement)
            , onMouseEnter (SelectProposedMovement movement)
            ]
    in
    li
        [ Attr.id
            ("interval-"
                ++ Degree.toString (Pitch.unwrapDegree interval.from)
                ++ "-"
                ++ Degree.toString (Pitch.unwrapDegree interval.to)
            )
        , Styles.flexRowCentered
        , if PitchSpaceData.isVertical display then
            Styles.height size

          else
            Styles.width size
        , Styles.transition
        , Attr.attributeIf (PitchSpaceData.positionIsVisible position) Styles.border
        , if PitchSpaceData.isVertical display then
            class "border-r-0"

          else
            class "border-b-0"
        ]
        [ (case movement of
            AscendTo pitch ->
                button
                    (onClick (SelectPitch (Just pitch) Nothing {- (Maybe.map DescendTo (Degree.step pitch -1)) -})
                        :: buttonAttrs
                    )
                    [ viewIntervalCharacter (Maybe.map Pitch.unwrapDegree pitchState.currentPitch) pitch
                    , moria
                    ]

            DescendTo pitch ->
                button
                    (onClick (SelectPitch (Just pitch) Nothing {- (Maybe.map AscendTo (Degree.step pitch 1)) -})
                        :: buttonAttrs
                    )
                    [ viewIntervalCharacter (Maybe.map Pitch.unwrapDegree pitchState.currentPitch) pitch
                    , moria
                    ]

            None ->
                div [ class "content-center" ]
                    [ moria ]
          )
            |> viewIf (PitchSpaceData.positionIsVisible position)
        ]


{-| TODO: We should have this take pitches, not just degrees. And probably don't
wrap in a maybe.
-}
viewIntervalCharacter : Maybe Degree -> Pitch -> Html Msg
viewIntervalCharacter currentPitch toPitch =
    currentPitch
        |> Maybe.map (\current -> Degree.getInterval current (Pitch.unwrapDegree toPitch))
        |> Maybe.andThen IntervalCharacter.basicInterval
        |> Maybe.map (IntervalCharacter.applyAccidental (Pitch.unwrapAccidental toPitch))
        |> Html.Extra.viewMaybe
            (ByzHtmlInterval.view >> List.singleton >> span [ class "me-2" ])


shouldHighlightInterval : PitchState -> Interval -> Bool
shouldHighlightInterval { currentPitch, proposedMovement } interval =
    Maybe.unwrap False
        (\current ->
            let
                currentPitchIndex =
                    Degree.indexOf current

                fromIndex =
                    Degree.indexOf (Pitch.unwrapDegree interval.from)

                toIndex =
                    Degree.indexOf (Pitch.unwrapDegree interval.to)
            in
            case proposedMovement of
                AscendTo degree ->
                    (currentPitchIndex < toIndex)
                        && (toIndex <= Degree.indexOf (Pitch.unwrapDegree degree))

                DescendTo degree ->
                    (currentPitchIndex > fromIndex)
                        && (fromIndex >= Degree.indexOf (Pitch.unwrapDegree degree))

                None ->
                    False
        )
        (Maybe.map Pitch.unwrapDegree currentPitch)



-- PITCH COLUMN


viewPitches : PitchSpaceData -> ModeSettings -> PitchState -> Html Msg
viewPitches pitchSpaceData modeSettings pitchState =
    let
        --     _ =
        --         Debug.log "in view pitches function" ""
        proposedMovementTo =
            Movement.unwrapTargetPitch pitchState.proposedMovement
    in
    Html.ol (listAttributes pitchSpaceData.display)
        (List.map
            (\degree ->
                Html.Lazy.lazy7 viewPitch
                    pitchSpaceData.display
                    pitchSpaceData.scalingFactor
                    pitchState
                    (PitchSpaceData.encodePitchPositionContext pitchSpaceData degree)
                    (Pitch.wrapDegree pitchState.currentPitch proposedMovementTo degree
                        |> Pitch.encode modeSettings.scale
                    )
                    (DegreeDataDict.get degree pitchSpaceData.pitchVisibility)
                    (DegreeDataDict.get degree pitchSpaceData.isonIndicators)
            )
            Degree.gamutList
        )


{-| Renders a single pitch element in the pitch space.


## Layout Parameters

  - `layout`: Determines if the pitch space is arranged vertically or horizontally
  - `scalingFactor`: Scaling factor for pitch element spacing
  - `pitchButtonSize`: Size of the pitch button element


## Mode and State

  - `modeSettings`: Current mode/scale configuration
  - `pitchState`: Current state of the pitch system (selected pitches, ison, etc.)


## Position Parameters

  - `pitchPositions`: String-encoded pitch positions of the degree, the degree above, and the degree below
  - `positionWithinRange`: Whether this pitch is within the visible range


## Pitch Data

  - `pitchString`: String representation of the pitch to display

-}
viewPitch :
    Display
    -> Float
    -> PitchState
    -> PitchPositionContextString
    -> PitchString
    -> PositionWithinVisibleRange
    -> IsonSelectionIndicator
    -> Html Msg
viewPitch display scalingFactor pitchState pitchPositions pitchString positionWithinRange isonStatusIndicator =
    let
        -- _ =
        --     Debug.log "in viewPitch" pitchString
        degree =
            Pitch.decode pitchString
                |> Result.map (Tuple.second >> Pitch.unwrapDegree)

        { pitchPosition, pitchPositionAbove, pitchPositionBelow } =
            Result.withDefault
                { pitchPosition = -1
                , pitchPositionBelow = Nothing
                , pitchPositionAbove = Nothing
                }
                (PitchSpaceData.decodePitchPositionContext pitchPositions)

        scale_ int =
            (toFloat int / 2) * scalingFactor

        size =
            case positionWithinRange of
                Below ->
                    0

                LowerBoundary ->
                    Maybe.map
                        (\above -> scale_ (above - pitchPosition))
                        pitchPositionAbove
                        |> Maybe.withDefault 0

                Within ->
                    Maybe.map2
                        (\below above ->
                            (toFloat (above - pitchPosition) / 2)
                                |> (+) (toFloat (pitchPosition - below) / 2)
                                |> (*) scalingFactor
                        )
                        pitchPositionBelow
                        pitchPositionAbove
                        |> Maybe.withDefault 0

                UpperBoundary ->
                    Maybe.map
                        (\below -> scale_ (pitchPosition - below))
                        pitchPositionBelow
                        |> Maybe.withDefault 0

                Above ->
                    0

        positionIsVisible =
            PitchSpaceData.positionIsVisible positionWithinRange

        showSpacingDetails =
            LayoutData.showSpacing && positionIsVisible

        attributeIfVisible =
            Attr.attributeIf positionIsVisible

        isIson =
            PitchSpaceData.isCurrentIson isonStatusIndicator
    in
    li
        ([ Attr.id ("pitch-" ++ Result.Extra.unwrap "err" Degree.toString degree)
         , Styles.transition
         , Attr.attributeIf showSpacingDetails Styles.border
         ]
            ++ (if PitchSpaceData.isVertical display then
                    [ Styles.height size
                    , attributeIfVisible Styles.flexRow
                    ]

                else
                    [ Styles.width size
                    , attributeIfVisible Styles.flexCol
                    ]
               )
        )
        [ viewIfLazy positionIsVisible
            (\_ ->
                pitchButton
                    display
                    scalingFactor
                    pitchState
                    pitchString
                    pitchPositions
                    positionWithinRange
                    isonStatusIndicator
            )
        , viewIfLazy isIson
            (\_ ->
                Result.Extra.unwrap Html.Extra.nothing
                    (\degree_ ->
                        Html.Lazy.lazy5 isonIndicator
                            display
                            scalingFactor
                            degree_
                            pitchPositions
                            positionWithinRange
                    )
                    degree
            )
        , viewIf showSpacingDetails
            (text (" (" ++ Round.round 2 size ++ "px)"))
        ]


{-| TODO: we still need to get the PitchState deconstructed or encoded into
primitives or referentially equivalent arguments. This means that if we store
the relevant data in a dict in PitchSpaceData, we'll need to ensure that updates
to the dict record are handled in a targeted manner rather than a blanket
re-init.

How much of the movement or pitch selection validation can we push off into the
pitch space data? This does seem closer to derived state rather than pure view.

-}
pitchButton :
    Display
    -> Float
    -> PitchState
    -> PitchString
    -> PitchPositionContextString
    -> PositionWithinVisibleRange
    -> IsonSelectionIndicator
    -> Html Msg
pitchButton display scalingFactor pitchState pitchString pitchPositions positionWithinRange isonStatusIndicator =
    let
        ( decodedScale, decodedPitch, decodedDegree ) =
            Result.Extra.unwrap ( Nothing, Nothing, Nothing )
                (\( s, p ) -> ( Just s, Just p, Just (Pitch.unwrapDegree p) ))
                (Pitch.decode pitchString)

        isCurrentDegree =
            decodedDegree == Maybe.map Pitch.unwrapDegree pitchState.currentPitch

        isCurrentPitch =
            proposedPitch == pitchState.currentPitch

        canBeSelectedAsIson =
            PitchSpaceData.canBeSelectedAsIson isonStatusIndicator

        position =
            pitchElementPosition display
                scalingFactor
                pitchPositions
                positionWithinRange
                PitchButton

        degreeCanSupportProposedAccidental =
            Maybe.map3 Pitch.isValidInflection
                decodedScale
                pitchState.proposedAccidental
                decodedDegree
                |> Maybe.withDefault False
                |> (&&) movementWouldBeValid

        movementWouldBeValid =
            Maybe.map3 Pitch.inflected
                decodedScale
                pitchState.proposedAccidental
                decodedDegree
                |> Maybe.andThen Result.toMaybe
                |> Maybe.map3 Pitch.getInterval
                    decodedScale
                    pitchState.currentPitch
                |> Maybe.map (Movement.ofInterval pitchState.currentPitch)
                |> Maybe.map3 Movement.isValid decodedScale pitchState.currentPitch
                |> Maybe.withDefault True

        applyAccidental maybeAccidental =
            Maybe.map2 (\scale pitch -> Pitch.applyAccidental scale pitch maybeAccidental)
                decodedScale
                decodedPitch

        proposedPitch =
            if degreeCanSupportProposedAccidental && movementWouldBeValid then
                applyAccidental pitchState.proposedAccidental

            else
                decodedPitch

        shouldHighlight =
            canBeSelectedAsIson || degreeCanSupportProposedAccidental

        proposedPitchIsInflected =
            Maybe.unwrap False Pitch.isInflected proposedPitch
    in
    button
        [ onClick <|
            if canBeSelectedAsIson then
                SetIson (Maybe.unwrap NoIson Selected decodedDegree)

            else if isCurrentDegree then
                if isCurrentPitch && proposedPitchIsInflected then
                    SelectPitch (applyAccidental Nothing) Nothing

                else if not isCurrentPitch && proposedPitchIsInflected then
                    SelectPitch proposedPitch Nothing

                else
                    SelectPitch Nothing Nothing

            else
                SelectPitch proposedPitch Nothing
        , pitchButtonSizeClass
        , class "rounded-full hover:z-20 cursor-pointer relative pb-8"
        , Styles.transition
        , if PitchSpaceData.isVertical display then
            Styles.top position

          else
            Styles.left position
        , classList
            [ ( "bg-red-200 z-10", isCurrentPitch )
            , ( "hover:text-green-700 bg-slate-200 hover:bg-slate-300 opacity-75 hover:opacity-90", not isCurrentPitch )
            , ( "text-green-700 bg-slate-300 z-10", Movement.unwrapTargetPitch pitchState.proposedMovement == decodedPitch )
            , ( "border-2 border-blue-700", shouldHighlight )
            , ( "border-2 border-transparent", not shouldHighlight )
            ]
        ]
        [ Html.Extra.viewMaybe
            (\accidental ->
                span [ class "absolute mt-2 md:mt-4", Styles.left 12 ]
                    [ Accidental.view Accidental.Red accidental ]
            )
            (Maybe.andThen Pitch.unwrapAccidental decodedPitch)
        , Maybe.map2
            (\scale pitch ->
                ByzHtmlMartyria.viewWithAttributes
                    [ Styles.left -3, Styles.top -3 ]
                    (Martyria.for scale (Pitch.unwrapDegree pitch))
            )
            decodedScale
            decodedPitch
            |> Maybe.withDefault Html.Extra.nothing
        ]


{-| Prepared for lazy rendering.
-}
isonIndicator : Display -> Float -> Degree -> PitchPositionContextString -> PositionWithinVisibleRange -> Html Msg
isonIndicator display scalingFactor degree pitchPositions positionWithinRange =
    let
        position =
            pitchElementPosition display
                scalingFactor
                pitchPositions
                positionWithinRange
                IsonIndicator
    in
    div
        (class "relative text-lg sm:text-2xl text-blue-700 text-greek"
            :: (if PitchSpaceData.isVertical display then
                    [ class "ml-3", Styles.top position ]

                else
                    [ class "mt-1", Styles.left position ]
               )
        )
        [ text "(", Degree.text degree, text ")" ]


type PitchElementTarget
    = PitchButton
    | IsonIndicator


pitchElementPosition :
    Display
    -> Float
    -> PitchPositionContextString
    -> PositionWithinVisibleRange
    -> PitchElementTarget
    -> Float
pitchElementPosition display scalingFactor pitchPositions positionWithinRange target =
    let
        { pitchPosition, pitchPositionAbove, pitchPositionBelow } =
            Result.withDefault
                { pitchPosition = -1
                , pitchPositionBelow = Nothing
                , pitchPositionAbove = Nothing
                }
                (PitchSpaceData.decodePitchPositionContext pitchPositions)

        scale int =
            (toFloat int / 2)
                |> (*) scalingFactor
                |> (+) (negate pitchButtonSizeValue)

        pitchButtonSizeValue =
            case target of
                PitchButton ->
                    PitchSpaceData.pitchButtonSize display / 2

                IsonIndicator ->
                    PitchSpaceData.pitchButtonSize display / 4
    in
    case ( PitchSpaceData.displayToLayout display, positionWithinRange ) of
        ( _, Below ) ->
            0

        ( Vertical, LowerBoundary ) ->
            Maybe.unwrap 0
                (\above -> scale (above - pitchPosition))
                pitchPositionAbove

        ( Horizontal, LowerBoundary ) ->
            negate pitchButtonSizeValue

        ( Vertical, Within ) ->
            Maybe.unwrap 0
                (\above -> scale (above - pitchPosition))
                pitchPositionAbove

        ( Horizontal, Within ) ->
            Maybe.unwrap 0
                (\below -> scale (pitchPosition - below))
                pitchPositionBelow

        ( Vertical, UpperBoundary ) ->
            negate pitchButtonSizeValue

        ( Horizontal, UpperBoundary ) ->
            Maybe.unwrap 0
                (\below -> scale (pitchPosition - below))
                pitchPositionBelow

        ( _, Above ) ->
            0
