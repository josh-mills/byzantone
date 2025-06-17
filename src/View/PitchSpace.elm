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
import Html exposing (Html, button, div, li, span, text)
import Html.Attributes as Attr exposing (class, classList)
import Html.Attributes.Extra as Attr
import Html.Events exposing (onClick, onFocus, onMouseEnter, onMouseLeave)
import Html.Extra exposing (viewIf, viewIfLazy)
import Maybe.Extra as Maybe
import Model exposing (Model)
import Model.LayoutData as LayoutData exposing (Layout(..), LayoutData)
import Model.ModeSettings exposing (ModeSettings)
import Model.PitchState as PitchState exposing (IsonStatus(..), PitchState)
import Movement exposing (Movement(..))
import Round
import Styles
import Update exposing (Msg(..))



-- WRAPPER AND VIEW HELPERS


view : Model -> Html Msg
view { layoutData, modeSettings, pitchState } =
    let
        layout =
            LayoutData.layoutFor layoutData

        visibleRange =
            calculateVisibleRange modeSettings pitchState

        params : Params
        params =
            { layout = layout
            , pitchButtonSize = calculatePitchButtonSize layoutData
            , layoutData = layoutData
            , modeSettings = modeSettings
            , pitchState = pitchState
            , scalingFactor = calculateScalingFactor layout layoutData modeSettings visibleRange
            , visibleRange = visibleRange
            }
    in
    div
        ([ Attr.id "pitch-space"
         , Styles.transition
         , Attr.attributeIf layoutData.showSpacing Styles.border
         ]
            ++ (case layout of
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
        [ viewIntervals params
        , viewPitches params
        ]


{-| Includes both state elements passed in from the model and also derived
values that are calculated from the state.
-}
type alias Params =
    { layout : Layout
    , layoutData : LayoutData
    , modeSettings : ModeSettings
    , pitchButtonSize : Float
    , pitchState : PitchState
    , scalingFactor : Float
    , visibleRange : { start : Pitch, end : Pitch }
    }


type alias PitchDisplayParams =
    { pitch : Pitch
    , pitchPosition : Int
    , pitchPositionAbove : Maybe Int
    , pitchPositionBelow : Maybe Int
    , positionWithinRange : PositionWithinVisibleRange
    , scalingFactor : Float
    }


{-| This feels potentially fragile.

TODO: we'll need some sort of minimum for the portrait to enable scrolling on
small viewports.

-}
calculateScalingFactor : Layout -> LayoutData -> ModeSettings -> { start : Pitch, end : Pitch } -> Float
calculateScalingFactor layout layoutData modeSettings visibleRange =
    let
        visibleRangeInMoria =
            Pitch.pitchPosition modeSettings.scale visibleRange.end
                - Pitch.pitchPosition modeSettings.scale visibleRange.start

        marginForButton =
            calculatePitchButtonSize layoutData
    in
    case layout of
        Vertical ->
            (layoutData.viewport.viewport.height
                - max layoutData.pitchSpace.element.y 128
                - marginForButton
            )
                / toFloat visibleRangeInMoria

        Horizontal ->
            (layoutData.pitchSpace.element.width
                - marginForButton
            )
                / toFloat visibleRangeInMoria


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


calculatePitchButtonSize : LayoutData -> Float
calculatePitchButtonSize layoutData =
    if layoutData.viewport.viewport.width < 640 then
        48

    else
        64



-- VISIBILITY


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



-- INTERVAL COLUMN


viewIntervals : Params -> Html Msg
viewIntervals params =
    Html.ol (onMouseLeave (SelectProposedMovement None) :: listAttributes params.layout)
        (List.map
            (viewInterval params)
            (intervalsWithVisibility params)
        )


intervalsWithVisibility : Params -> List ( Interval, PositionWithinVisibleRange )
intervalsWithVisibility params =
    let
        lowerBoundIndex =
            Degree.indexOf (Pitch.unwrapDegree params.visibleRange.start)

        upperBoundIndex =
            Degree.indexOf (Pitch.unwrapDegree params.visibleRange.end)

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
        (Pitch.intervals params.modeSettings.scale
            params.pitchState.currentPitch
            (Movement.toPitch params.pitchState.proposedMovement)
        )


{-| TODO: see if with some refactors, we could use Html.Lazy for this. But
benchmark this before shipping.
-}
viewInterval : Params -> ( Interval, PositionWithinVisibleRange ) -> Html Msg
viewInterval { layout, layoutData, pitchState, scalingFactor } ( interval, position ) =
    let
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
                , viewIf layoutData.showSpacing
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
        , Attr.attributeIf (positionIsVisible position) Styles.border
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
            |> viewIf (positionIsVisible position)
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


viewPitches : Params -> Html Msg
viewPitches params =
    Html.ol (listAttributes params.layout)
        (List.map (viewPitch params) (pitchesWithVisibility params))


pitchesWithVisibility : Params -> List ( Pitch, PositionWithinVisibleRange )
pitchesWithVisibility params =
    let
        lowerBoundIndex =
            Degree.indexOf (Pitch.unwrapDegree params.visibleRange.start)

        upperBoundIndex =
            Degree.indexOf (Pitch.unwrapDegree params.visibleRange.end)

        visibility pitchIndex =
            if pitchIndex < lowerBoundIndex then
                Below

            else if pitchIndex > upperBoundIndex then
                Above

            else if pitchIndex == lowerBoundIndex then
                LowerBoundary

            else if pitchIndex == upperBoundIndex then
                UpperBoundary

            else
                Within
    in
    List.map
        (\degree ->
            ( Pitch.wrapDegree params.pitchState.currentPitch
                (Movement.toPitch params.pitchState.proposedMovement)
                degree
            , visibility (Degree.indexOf degree)
            )
        )
        Degree.gamutList


viewPitch : Params -> ( Pitch, PositionWithinVisibleRange ) -> Html Msg
viewPitch ({ layout, layoutData, modeSettings, scalingFactor } as params) ( pitch, positionWithinRange ) =
    let
        degree =
            Pitch.unwrapDegree pitch

        pitchPosition : Int
        pitchPosition =
            Pitch.pitchPosition modeSettings.scale pitch

        inflectedPitchPosition : Degree -> Int
        inflectedPitchPosition =
            Pitch.wrapDegree params.pitchState.currentPitch
                (Movement.toPitch params.pitchState.proposedMovement)
                >> Pitch.pitchPosition modeSettings.scale

        pitchPositionAbove : Maybe Int
        pitchPositionAbove =
            Degree.step degree 1
                |> Maybe.map inflectedPitchPosition

        pitchPositionBelow : Maybe Int
        pitchPositionBelow =
            Degree.step degree -1
                |> Maybe.map inflectedPitchPosition

        pitchDisplayParams : PitchDisplayParams
        pitchDisplayParams =
            -- TODO: we'll need to feed in the accidental somehow.
            { pitch = pitch
            , pitchPosition = pitchPosition
            , pitchPositionAbove = pitchPositionAbove
            , pitchPositionBelow = pitchPositionBelow
            , positionWithinRange = positionWithinRange
            , scalingFactor = scalingFactor
            }

        scale int =
            (toFloat int / 2) * scalingFactor

        size =
            case positionWithinRange of
                Below ->
                    0

                LowerBoundary ->
                    Maybe.map
                        (\above -> scale (above - pitchPosition))
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
                        (\below -> scale (pitchPosition - below))
                        pitchPositionBelow
                        |> Maybe.withDefault 0

                Above ->
                    0

        showSpacingDetails =
            layoutData.showSpacing && positionIsVisible positionWithinRange

        attributeIfVisible =
            Attr.attributeIf (positionIsVisible positionWithinRange)

        isIson =
            PitchState.ison params.pitchState == Just (Pitch.natural degree)
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
        [ viewIfLazy (positionIsVisible positionWithinRange)
            (\_ -> pitchButton params pitchDisplayParams)
        , viewIfLazy isIson
            (\_ -> isonIndicator params pitchDisplayParams)
        , viewIf showSpacingDetails
            (text (" (" ++ Round.round 2 size ++ "px)"))
        ]


pitchButton : Params -> PitchDisplayParams -> Html Msg
pitchButton ({ layout, modeSettings, pitchState } as params) ({ pitch } as pitchDisplayParams) =
    let
        isCurrentDegree =
            Just (Pitch.unwrapDegree pitch) == Maybe.map Pitch.unwrapDegree pitchState.currentPitch

        isCurrentPitch =
            Just proposedPitch == pitchState.currentPitch

        canBeSelectedAsIson =
            -- this should be made a bit more robust once modal logic gets built out.
            case pitchState.ison of
                SelectingIson _ ->
                    True

                _ ->
                    False

        position =
            pitchElementPosition params pitchDisplayParams PitchButton

        degreeCanSupportProposedAccidental =
            Maybe.map
                (\accidental ->
                    Pitch.isValid
                        params.modeSettings.scale
                        accidental
                        (Pitch.unwrapDegree pitch)
                )
                pitchState.proposedAccidental

        proposedPitch =
            if Maybe.withDefault False degreeCanSupportProposedAccidental then
                Pitch.applyAccidental modeSettings.scale pitchState.proposedAccidental pitch

            else
                pitch

        shouldHighlight =
            canBeSelectedAsIson
                || Maybe.withDefault False degreeCanSupportProposedAccidental
    in
    button
        [ onClick <|
            if canBeSelectedAsIson then
                SetIson (Selected (Pitch.unwrapDegree pitch))

            else if isCurrentDegree then
                if isCurrentPitch && Pitch.isInflected proposedPitch then
                    SelectPitch (Just (Pitch.applyAccidental modeSettings.scale Nothing pitch)) Nothing

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
            , ( "text-green-700 bg-slate-300 z-10", Movement.toPitch pitchState.proposedMovement == Just pitch )
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
            (Martyria.for modeSettings.scale (Pitch.unwrapDegree pitch))
        ]


isonIndicator : Params -> PitchDisplayParams -> Html Msg
isonIndicator ({ layout } as params) ({ pitch } as pitchDisplayParams) =
    let
        position =
            pitchElementPosition params pitchDisplayParams IsonIndicator
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
        [ text "(", Degree.text (Pitch.unwrapDegree pitch), text ")" ]


type PitchElementTarget
    = PitchButton
    | IsonIndicator


pitchElementPosition : Params -> PitchDisplayParams -> PitchElementTarget -> Float
pitchElementPosition { layout, pitchButtonSize } { pitchPosition, pitchPositionAbove, pitchPositionBelow, positionWithinRange, scalingFactor } target =
    let
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
