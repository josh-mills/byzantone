module View.PitchSpace exposing (view)

{-| View logic for pitch space (i.e., the intervalic space and positioned pitches)
-}

import Byzantine.ByzHtml.Interval as ByzHtmlInterval
import Byzantine.ByzHtml.Martyria as ByzHtmlMartyria
import Byzantine.Degree as Degree exposing (Degree)
import Byzantine.IntervalCharacter as IntervalCharacter
import Byzantine.Martyria as Martyria
import Byzantine.Pitch as Pitch exposing (Interval)
import Html exposing (Html, button, div, li, span, text)
import Html.Attributes as Attr exposing (class, classList)
import Html.Attributes.Extra as Attr
import Html.Events exposing (onClick, onFocus, onMouseEnter, onMouseLeave)
import Html.Extra exposing (viewIf, viewIfLazy)
import Maybe.Extra as Maybe
import Model exposing (Layout(..), LayoutData, Model, PitchState, layoutFor)
import Model.ModeSettings exposing (ModeSettings)
import Movement exposing (Movement(..))
import Round
import Styles
import Update exposing (Msg(..))



-- HELPER TYPES AND FUNCTIONS


type alias Params =
    { layoutData : LayoutData
    , modeSettings : ModeSettings
    , pitchState : PitchState
    }


{-| What is the visible range of the pitch space? This expands the default (or
user-set) start and stop positions to include the current pitch. (We may want to
consider additional limits as well.)
-}
visibleRange : Params -> { start : Degree, end : Degree }
visibleRange { modeSettings, pitchState } =
    case pitchState.currentPitch of
        Just currentPitch ->
            { start =
                if Degree.indexOf currentPitch < Degree.indexOf modeSettings.rangeStart then
                    currentPitch

                else
                    modeSettings.rangeStart
            , end =
                if Degree.indexOf currentPitch > Degree.indexOf modeSettings.rangeEnd then
                    currentPitch

                else
                    modeSettings.rangeEnd
            }

        Nothing ->
            { start = modeSettings.rangeStart
            , end = modeSettings.rangeEnd
            }


visibleRangeInMoria : Params -> Int
visibleRangeInMoria params =
    let
        { start, end } =
            visibleRange params
    in
    Pitch.pitchPosition params.modeSettings.scale end - Pitch.pitchPosition params.modeSettings.scale start


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


pitchesWithVisibility : Params -> List ( Degree, PositionWithinVisibleRange )
pitchesWithVisibility params =
    let
        { start, end } =
            visibleRange params

        lowerBoundIndex =
            Degree.indexOf start

        upperBoundIndex =
            Degree.indexOf end

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
            ( degree
            , visibility (Degree.indexOf degree)
            )
        )
        Degree.gamutList


intervalsWithVisibility : Params -> List ( Interval, PositionWithinVisibleRange )
intervalsWithVisibility params =
    let
        { start, end } =
            visibleRange params

        lowerBoundIndex =
            Degree.indexOf start

        upperBoundIndex =
            Degree.indexOf end

        visibility interval =
            let
                intervalFromIndex =
                    Degree.indexOf interval.from

                intervalToIndex =
                    Degree.indexOf interval.to
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
        (Pitch.intervals params.modeSettings.scale)


{-| This feels potentially fragile.

TODO: we'll need some sort of minimum for the portrait to enable scrolling on
small viewports.

-}
scalingFactor : Params -> Float
scalingFactor ({ layoutData } as params) =
    let
        rangeInMoria =
            visibleRangeInMoria params

        marginForButton =
            pitchButtonSize layoutData
    in
    case layoutFor layoutData of
        Vertical ->
            (layoutData.viewport.viewport.height
                - max layoutData.pitchSpace.element.y 128
                - marginForButton
            )
                / toFloat rangeInMoria

        Horizontal ->
            (layoutData.pitchSpace.element.width
                - marginForButton
            )
                / toFloat rangeInMoria



-- WRAPPER


view : Model -> Html Msg
view { layoutData, modeSettings, pitchState } =
    let
        params =
            { layoutData = layoutData
            , modeSettings = modeSettings
            , pitchState = pitchState
            }
    in
    div
        ([ Attr.id "pitch-space"
         , Styles.transition
         , Attr.attributeIf layoutData.showSpacing Styles.border
         ]
            ++ (case layoutFor layoutData of
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


listAttributes : LayoutData -> List (Html.Attribute Msg)
listAttributes layoutData =
    case layoutFor layoutData of
        Vertical ->
            [ class "flex flex-col-reverse justify-end w-36 mx-4" ]

        Horizontal ->
            [ Styles.flexRowCentered
            , class "h-24 w-full my-4"
            ]



-- INTERVAL COLUMN


viewIntervals : Params -> Html Msg
viewIntervals params =
    Html.ol (onMouseLeave (SelectProposedMovement None) :: listAttributes params.layoutData)
        (List.map
            (viewInterval params)
            (intervalsWithVisibility params)
        )


{-| TODO: see if with some refactors, we could use Html.Lazy for this. But
benchmark this before shipping.
-}
viewInterval : Params -> ( Interval, PositionWithinVisibleRange ) -> Html Msg
viewInterval ({ layoutData, pitchState } as params) ( interval, position ) =
    let
        size =
            case position of
                Above ->
                    0

                Below ->
                    0

                _ ->
                    toFloat interval.moria * scalingFactor params

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
        [ Attr.id ("interval-" ++ Degree.toString interval.from ++ "-" ++ Degree.toString interval.to)
        , Styles.flexRowCentered
        , case layoutFor layoutData of
            Vertical ->
                Styles.height size

            Horizontal ->
                Styles.width size
        , Styles.transition
        , Attr.attributeIf (positionIsVisible position) Styles.border
        ]
        [ (case movement of
            AscendTo degree ->
                button
                    (onClick (SelectPitch (Just degree) (Maybe.map DescendTo (Degree.step degree -1)))
                        :: buttonAttrs
                    )
                    [ viewIntervalCharacter pitchState.currentPitch degree
                    , moria
                    ]

            DescendTo degree ->
                button
                    (onClick (SelectPitch (Just degree) (Maybe.map AscendTo (Degree.step degree 1)))
                        :: buttonAttrs
                    )
                    [ viewIntervalCharacter pitchState.currentPitch degree
                    , moria
                    ]

            None ->
                div [ class "content-center" ]
                    [ moria ]
          )
            |> viewIf (positionIsVisible position)
        ]


viewIntervalCharacter : Maybe Degree -> Degree -> Html Msg
viewIntervalCharacter currentPitch degree =
    currentPitch
        |> Maybe.map (\current -> Degree.getInterval current degree)
        |> Maybe.andThen IntervalCharacter.basicInterval
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
                    Degree.indexOf interval.from

                toIndex =
                    Degree.indexOf interval.to
            in
            case proposedMovement of
                AscendTo degree ->
                    (currentPitchIndex < toIndex)
                        && (toIndex <= Degree.indexOf degree)

                DescendTo degree ->
                    (currentPitchIndex > fromIndex)
                        && (fromIndex >= Degree.indexOf degree)

                None ->
                    False
        )
        currentPitch



-- PITCH COLUMN


viewPitches : Params -> Html Msg
viewPitches params =
    Html.ol (listAttributes params.layoutData)
        (List.map
            (viewPitch params (scalingFactor params))
            (pitchesWithVisibility params)
        )


viewPitch : Params -> Float -> ( Degree, PositionWithinVisibleRange ) -> Html Msg
viewPitch ({ layoutData, modeSettings } as params) scalingFactor_ ( degree, positionWithinRange ) =
    let
        pitch =
            Pitch.pitchPosition modeSettings.scale degree

        pitchBelow =
            Degree.step degree -1
                |> Maybe.map (Pitch.pitchPosition modeSettings.scale)

        pitchAbove =
            Degree.step degree 1
                |> Maybe.map (Pitch.pitchPosition modeSettings.scale)

        scale int =
            (toFloat int / 2) * scalingFactor_

        size =
            case positionWithinRange of
                Below ->
                    0

                LowerBoundary ->
                    Maybe.map
                        (\above -> scale (above - pitch))
                        pitchAbove
                        |> Maybe.withDefault 0

                Within ->
                    Maybe.map2
                        (\below above ->
                            (toFloat (above - pitch) / 2)
                                |> (+) (toFloat (pitch - below) / 2)
                                |> (*) scalingFactor_
                        )
                        pitchBelow
                        pitchAbove
                        |> Maybe.withDefault 0

                UpperBoundary ->
                    Maybe.map
                        (\below -> scale (pitch - below))
                        pitchBelow
                        |> Maybe.withDefault 0

                Above ->
                    0

        showSpacingDetails =
            layoutData.showSpacing && positionIsVisible positionWithinRange

        attributeIfVisible =
            Attr.attributeIf (positionIsVisible positionWithinRange)
    in
    li
        [ Attr.id ("pitch-" ++ Degree.toString degree)
        , Styles.transition
        , Attr.attributeIf showSpacingDetails Styles.border
        , attributeIfVisible Styles.flexCol
        , case layoutFor layoutData of
            Vertical ->
                Styles.height size

            Horizontal ->
                Styles.width size
        ]
        [ viewIfLazy (positionIsVisible positionWithinRange)
            (\_ ->
                pitchButton params
                    { degree = degree
                    , pitch = pitch
                    , pitchAbove = pitchAbove
                    , pitchBelow = pitchBelow
                    , positionWithinRange = positionWithinRange
                    , scalingFactor_ = scalingFactor_
                    }
            )
        , viewIf showSpacingDetails
            (text (" (" ++ Round.round 2 size ++ "px)"))
        ]


pitchButton :
    Params
    ->
        { degree : Degree
        , pitch : Int
        , pitchAbove : Maybe Int
        , pitchBelow : Maybe Int
        , positionWithinRange : PositionWithinVisibleRange
        , scalingFactor_ : Float
        }
    -> Html Msg
pitchButton { layoutData, modeSettings, pitchState } { degree, pitch, pitchAbove, pitchBelow, positionWithinRange, scalingFactor_ } =
    let
        isCurrentPitch =
            Just degree == pitchState.currentPitch

        layout =
            layoutFor layoutData

        scale int =
            (toFloat int / 2)
                |> (*) scalingFactor_
                |> (+) (negate pitchButtonSizeValue)

        position =
            case ( layout, positionWithinRange ) of
                ( _, Below ) ->
                    0

                ( Vertical, LowerBoundary ) ->
                    Maybe.map
                        (\above -> scale (above - pitch))
                        pitchAbove
                        |> Maybe.withDefault 0

                ( Horizontal, LowerBoundary ) ->
                    negate pitchButtonSizeValue

                ( Vertical, Within ) ->
                    Maybe.map
                        (\above -> scale (above - pitch))
                        pitchAbove
                        |> Maybe.withDefault 0

                ( Horizontal, Within ) ->
                    Maybe.map
                        (\below -> scale (pitch - below))
                        pitchBelow
                        |> Maybe.withDefault 0

                ( Vertical, UpperBoundary ) ->
                    negate pitchButtonSizeValue

                ( Horizontal, UpperBoundary ) ->
                    Maybe.map
                        (\below -> scale (pitch - below))
                        pitchBelow
                        |> Maybe.withDefault 0

                ( _, Above ) ->
                    0

        pitchButtonSizeValue =
            pitchButtonSize layoutData / 2
    in
    button
        [ onClick <|
            if isCurrentPitch then
                SelectPitch Nothing Nothing

            else
                SelectPitch (Just degree) Nothing
        , pitchButtonSizeClass
        , class "rounded-full hover:z-10 cursor-pointer relative pb-8"
        , Styles.transition
        , case layoutFor layoutData of
            Vertical ->
                Styles.top position

            Horizontal ->
                Styles.left position
        , classList
            [ ( "bg-red-200", isCurrentPitch )
            , ( "hover:text-green-700 bg-slate-200 hover:bg-slate-300 opacity-75 hover:opacity-100", not isCurrentPitch )
            , ( "text-green-700 bg-slate-300 z-10", Movement.toDegree pitchState.proposedMovement == Just degree )
            ]
        ]
        [ ByzHtmlMartyria.viewWithAttributes
            [ Styles.left -3, Styles.top -3 ]
            (Martyria.for modeSettings.scale degree)
        ]


{-| 64px default, 48px below the sm breakpoint.
-}
pitchButtonSizeClass : Html.Attribute msg
pitchButtonSizeClass =
    class "w-12 h-12 sm:w-16 sm:h-16 text-xl sm:text-3xl"


pitchButtonSize : LayoutData -> Float
pitchButtonSize layoutData =
    if layoutData.viewport.viewport.width < 640 then
        48

    else
        64
