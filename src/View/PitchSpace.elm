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
import Byzantine.Pitch as Pitch exposing (Interval, Pitch)
import Byzantine.Scale exposing (Scale)
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
import Model.PitchSpaceData as PitchSpaceData exposing (IsonSelectionIndicator, PitchSpaceData, PositionWithinVisibleRange(..), calculateVisibleRange)
import Model.PitchState as PitchState exposing (IsonStatus(..), PitchState)
import Movement exposing (Movement(..))
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
            ++ (case pitchSpaceData.layout of
                    Vertical ->
                        [ Styles.flexRow
                        , class "my-8"
                        ]

                    Horizontal ->
                        [ Styles.flexCol
                        , class "mx-8"
                        ]
               )
        )
        [ viewIntervals pitchSpaceData modeSettings pitchState
        , viewPitches pitchSpaceData modeSettings pitchState
        ]


listAttributes : Layout -> List (Html.Attribute Msg)
listAttributes layout =
    case layout of
        Vertical ->
            [ class "flex flex-col-reverse justify-end w-36 mx-4" ]

        Horizontal ->
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
    Html.ol (onMouseLeave (SelectProposedMovement None) :: listAttributes pitchSpaceData.layout)
        (List.map
            (viewInterval pitchSpaceData.layout pitchSpaceData.scalingFactor pitchState)
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


viewInterval : Layout -> Float -> PitchState -> ( Interval, PositionWithinVisibleRange ) -> Html Msg
viewInterval layout scalingFactor pitchState ( interval, position ) =
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
        , case layout of
            Vertical ->
                Styles.height size

            Horizontal ->
                Styles.width size
        , Styles.transition
        , Attr.attributeIf (PitchSpaceData.positionIsVisible position) Styles.border
        , case layout of
            Vertical ->
                class "border-r-0"

            Horizontal ->
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
    Html.ol (listAttributes pitchSpaceData.layout)
        (List.map
            (\degree ->
                -- Html.Lazy.lazy8
                viewPitch
                    pitchSpaceData.layout
                    pitchSpaceData.scalingFactor
                    pitchSpaceData.pitchButtonSize
                    modeSettings.scale
                    pitchState
                    (PitchSpaceData.encodePitchPositionContext pitchSpaceData degree)
                    (Pitch.wrapDegree pitchState.currentPitch proposedMovementTo degree
                        |> Pitch.encode
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
    Layout
    -> Float
    -> Float
    -> Scale
    -> PitchState
    -> String
    -> String
    -> PositionWithinVisibleRange
    -> IsonSelectionIndicator
    -> Html Msg
viewPitch layout scalingFactor pitchButtonSize scale pitchState pitchPositions pitchString positionWithinRange isonStatusIndicator =
    let
        -- _ =
        --     Debug.log "in viewPitch" pitchString
        pitch =
            Pitch.decodeWithDefault scale pitchString

        degree =
            Pitch.unwrapDegree pitch

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
        ([ Attr.id ("pitch-" ++ Degree.toString degree)
         , Styles.transition
         , Attr.attributeIf showSpacingDetails Styles.border
         ]
            ++ (case layout of
                    Vertical ->
                        [ Styles.height size
                        , attributeIfVisible Styles.flexRow
                        ]

                    Horizontal ->
                        [ Styles.width size
                        , attributeIfVisible Styles.flexCol
                        ]
               )
        )
        [ viewIfLazy positionIsVisible
            (\_ ->
                pitchButton
                    layout
                    scalingFactor
                    pitchButtonSize
                    scale
                    pitchState
                    pitchString
                    pitchPositions
                    positionWithinRange
                    isonStatusIndicator
            )
        , viewIfLazy isIson
            (\_ ->
                Html.Lazy.lazy6 isonIndicator
                    layout
                    scalingFactor
                    pitchButtonSize
                    degree
                    pitchPositions
                    positionWithinRange
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
pitchButton : Layout -> Float -> Float -> Scale -> PitchState -> String -> String -> PositionWithinVisibleRange -> IsonSelectionIndicator -> Html Msg
pitchButton layout scalingFactor pitchButtonSize scale pitchState pitchString pitchPositions positionWithinRange isonStatusIndicator =
    let
        pitch =
            Pitch.decodeWithDefault scale pitchString

        isCurrentDegree =
            Just (Pitch.unwrapDegree pitch) == Maybe.map Pitch.unwrapDegree pitchState.currentPitch

        isCurrentPitch =
            Just proposedPitch == pitchState.currentPitch

        canBeSelectedAsIson =
            PitchSpaceData.canBeSelectedAsIson isonStatusIndicator

        position =
            pitchElementPosition layout
                scalingFactor
                pitchButtonSize
                pitchPositions
                positionWithinRange
                PitchButton

        degreeCanSupportProposedAccidental =
            Maybe.map
                (\accidental ->
                    Pitch.isValidInflection
                        scale
                        accidental
                        (Pitch.unwrapDegree pitch)
                )
                pitchState.proposedAccidental
                |> Maybe.withDefault False
                |> (&&) movementWouldBeValid

        movementWouldBeValid =
            pitchState.proposedAccidental
                |> Maybe.andThen
                    (\accidental ->
                        Pitch.inflected scale
                            accidental
                            (Pitch.unwrapDegree pitch)
                            |> Result.toMaybe
                    )
                |> Maybe.map2
                    (Pitch.getInterval scale)
                    pitchState.currentPitch
                |> Maybe.map (Movement.ofInterval pitchState.currentPitch)
                |> Maybe.map2 (Movement.isValid scale) pitchState.currentPitch
                |> Maybe.withDefault True

        proposedPitch =
            if degreeCanSupportProposedAccidental && movementWouldBeValid then
                Pitch.applyAccidental scale pitchState.proposedAccidental pitch

            else
                pitch

        shouldHighlight =
            canBeSelectedAsIson || degreeCanSupportProposedAccidental
    in
    button
        [ onClick <|
            if canBeSelectedAsIson then
                SetIson (Selected (Pitch.unwrapDegree pitch))

            else if isCurrentDegree then
                if isCurrentPitch && Pitch.isInflected proposedPitch then
                    SelectPitch (Just (Pitch.applyAccidental scale Nothing pitch)) Nothing

                else if not isCurrentPitch && Pitch.isInflected proposedPitch then
                    SelectPitch (Just proposedPitch) Nothing

                else
                    SelectPitch Nothing Nothing

            else
                SelectPitch (Just proposedPitch) Nothing
        , pitchButtonSizeClass
        , class "rounded-full hover:z-20 cursor-pointer relative pb-8"
        , Styles.transition
        , case layout of
            Vertical ->
                Styles.top position

            Horizontal ->
                Styles.left position
        , classList
            [ ( "bg-red-200 z-10", isCurrentPitch )
            , ( "hover:text-green-700 bg-slate-200 hover:bg-slate-300 opacity-75 hover:opacity-90", not isCurrentPitch )
            , ( "text-green-700 bg-slate-300 z-10", Movement.unwrapTargetPitch pitchState.proposedMovement == Just pitch )
            , ( "border-2 border-blue-700", shouldHighlight )
            , ( "border-2 border-transparent", not shouldHighlight )
            ]
        ]
        [ Html.Extra.viewMaybe
            (\accidental ->
                span [ class "absolute mt-2 md:mt-4", Styles.left 12 ]
                    [ Accidental.view Accidental.Red accidental ]
            )
            (Pitch.unwrapAccidental pitch)
        , ByzHtmlMartyria.viewWithAttributes
            [ Styles.left -3, Styles.top -3 ]
            (Martyria.for scale (Pitch.unwrapDegree pitch))
        ]


{-| Prepared for lazy rendering.
-}
isonIndicator : Layout -> Float -> Float -> Degree -> String -> PositionWithinVisibleRange -> Html Msg
isonIndicator layout scalingFactor pitchButtonSize degree pitchPositions positionWithinRange =
    let
        position =
            pitchElementPosition layout
                scalingFactor
                pitchButtonSize
                pitchPositions
                positionWithinRange
                IsonIndicator
    in
    div
        (class "relative text-lg sm:text-2xl text-blue-700 text-greek"
            :: (case layout of
                    Vertical ->
                        [ class "ml-3"
                        , Styles.top position
                        ]

                    Horizontal ->
                        [ class "mt-1"
                        , Styles.left position
                        ]
               )
        )
        [ text "(", Degree.text degree, text ")" ]


type PitchElementTarget
    = PitchButton
    | IsonIndicator


pitchElementPosition :
    Layout
    -> Float
    -> Float
    -> String
    -> PositionWithinVisibleRange
    -> PitchElementTarget
    -> Float
pitchElementPosition layout scalingFactor pitchButtonSize pitchPositions positionWithinRange target =
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
                    pitchButtonSize / 2

                IsonIndicator ->
                    pitchButtonSize / 4
    in
    case ( layout, positionWithinRange ) of
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
