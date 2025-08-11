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
        proposedMovementTo =
            Movement.unwrapTargetPitch pitchState.proposedMovement
    in
    Html.ol (listAttributes pitchSpaceData.display)
        (List.map
            (\degree ->
                Html.Lazy.lazy8 viewPitch
                    (Pitch.wrapDegree pitchState.currentPitch proposedMovementTo degree
                        |> Pitch.encode modeSettings.scale
                    )
                    (Just degree == Maybe.map Pitch.unwrapDegree pitchState.currentPitch)
                    pitchSpaceData.display
                    (DegreeDataDict.get degree pitchSpaceData.isonIndicators)
                    (PitchSpaceData.encodePitchPositionContext pitchSpaceData degree)
                    (DegreeDataDict.get degree pitchSpaceData.pitchToSelect
                        |> Maybe.unwrap "nothing" (Pitch.encode modeSettings.scale)
                    )
                    (DegreeDataDict.get degree pitchSpaceData.pitchVisibility)
                    pitchSpaceData.scalingFactor
            )
            Degree.gamutList
        )


{-| Renders a single pitch element in the pitch space. Prepared for lazy
rendering.
-}
viewPitch :
    PitchString
    -> Bool
    -> Display
    -> IsonSelectionIndicator
    -> PitchPositionContextString
    -> PitchString
    -> PositionWithinVisibleRange
    -> Float
    -> Html Msg
viewPitch pitchString isCurrentDegree display isonStatusIndicator pitchPositions pitchToSelect positionWithinRange scalingFactor =
    let
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
                -- lazy rendering not needed; this is covered by the lazy check on `viewPitch`.
                pitchButton
                    pitchString
                    isCurrentDegree
                    display
                    isonStatusIndicator
                    pitchPositions
                    pitchToSelect
                    positionWithinRange
                    scalingFactor
            )
        , viewIfLazy isIson
            (\_ ->
                Result.Extra.unwrap Html.Extra.nothing
                    (\degree_ ->
                        Html.Lazy.lazy5 isonIndicator
                            degree_
                            display
                            pitchPositions
                            positionWithinRange
                            scalingFactor
                    )
                    degree
            )
        , viewIf showSpacingDetails
            (text (" (" ++ Round.round 2 size ++ "px)"))
        ]


{-| Prepared for lazy rendering.

TODO: To enable lazy rendering, we're dropping the proposed movement argument
(previously included with the PitchState argument), which means that the pitch
won't highlight when it is the target of the proposed movement. If this behavior
is still desirable, we'll need a way of getting this back in.

-}
pitchButton :
    PitchString
    -> Bool
    -> Display
    -> IsonSelectionIndicator
    -> PitchPositionContextString
    -> PitchString
    -> PositionWithinVisibleRange
    -> Float
    -> Html Msg
pitchButton pitchString isCurrentDegree display isonStatusIndicator pitchPositions pitchToSelect positionWithinRange scalingFactor =
    let
        ( decodedScale, decodedPitch, decodedDegree ) =
            Result.Extra.unwrap ( Nothing, Nothing, Nothing )
                (\( s, p ) -> ( Just s, Just p, Just (Pitch.unwrapDegree p) ))
                (Pitch.decode pitchString)

        decodedPitchToSelect =
            Pitch.decode pitchToSelect
                |> Result.toMaybe
                |> Maybe.map Tuple.second

        canBeSelectedAsIson =
            PitchSpaceData.canBeSelectedAsIson isonStatusIndicator

        position =
            pitchElementPosition PitchButton
                display
                pitchPositions
                positionWithinRange
                scalingFactor

        shouldHighlight =
            canBeSelectedAsIson || Maybe.unwrap False Pitch.isInflected decodedPitchToSelect
    in
    button
        [ onClick <|
            if canBeSelectedAsIson then
                SetIson (Maybe.unwrap NoIson Selected decodedDegree)

            else
                SelectPitch decodedPitchToSelect Nothing
        , pitchButtonSizeClass
        , class "rounded-full hover:z-20 cursor-pointer relative pb-8"
        , Styles.transition
        , if PitchSpaceData.isVertical display then
            Styles.top position

          else
            Styles.left position
        , classList
            [ ( "bg-red-200 z-10", isCurrentDegree )
            , ( "hover:text-green-700 bg-slate-200 hover:bg-slate-300 opacity-75 hover:opacity-90", not isCurrentDegree )

            -- , ( "text-green-700 bg-slate-300 z-10", Movement.unwrapTargetPitch pitchState.proposedMovement == decodedPitch )
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
isonIndicator : Degree -> Display -> PitchPositionContextString -> PositionWithinVisibleRange -> Float -> Html Msg
isonIndicator degree display pitchPositions positionWithinRange scalingFactor =
    let
        position =
            pitchElementPosition IsonIndicator
                display
                pitchPositions
                positionWithinRange
                scalingFactor
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
    PitchElementTarget
    -> Display
    -> PitchPositionContextString
    -> PositionWithinVisibleRange
    -> Float
    -> Float
pitchElementPosition target display pitchPositions positionWithinRange scalingFactor =
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
