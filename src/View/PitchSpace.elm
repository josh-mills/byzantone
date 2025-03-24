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
import Html.Events exposing (onClick, onFocus, onMouseEnter)
import Html.Extra exposing (viewIf, viewIfLazy)
import Html.Keyed
import Maybe.Extra as Maybe
import Model exposing (Layout(..), LayoutData, Model, layoutFor)
import Movement exposing (Movement(..))
import Styles
import Update exposing (Msg(..))



-- HELPER TYPES AND FUNCTIONS


{-| What is the visible range of the pitch space? This expands the default (or
user-set) start and stop positions to include the current pitch. (We may want to
consider additional limits as well.)
-}
visibleRange : Model -> { start : Degree, end : Degree }
visibleRange model =
    case model.currentPitch of
        Just currentPitch ->
            { start =
                if Degree.indexOf currentPitch < Degree.indexOf model.rangeStart then
                    currentPitch

                else
                    model.rangeStart
            , end =
                if Degree.indexOf currentPitch > Degree.indexOf model.rangeEnd then
                    currentPitch

                else
                    model.rangeEnd
            }

        Nothing ->
            { start = model.rangeStart
            , end = model.rangeEnd
            }


visibleRangeInMoria : Model -> Int
visibleRangeInMoria model =
    let
        { start, end } =
            visibleRange model
    in
    Pitch.pitchPosition model.scale end - Pitch.pitchPosition model.scale start


type PositionWithinVisibleRange
    = Below
    | LowerBoundary
    | Within
    | UpperBoundary
    | Above


{-| not actually needed; just for fun
-}
positionString : PositionWithinVisibleRange -> String
positionString position =
    case position of
        Below ->
            "Below"

        LowerBoundary ->
            "LowerBoundary"

        Within ->
            "Within"

        UpperBoundary ->
            "UpperBoundary"

        Above ->
            "Above"


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


{-| TODO: refactor to process as a list so it's not re-computing the visible
range over and over.
-}
pitchPositionWithinVisibleRange : Model -> Degree -> PositionWithinVisibleRange
pitchPositionWithinVisibleRange model degree =
    let
        pitchIndex =
            Degree.indexOf degree

        { start, end } =
            visibleRange model

        lowerBoundIndex =
            Degree.indexOf start

        upperBoundIndex =
            Degree.indexOf end
    in
    if pitchIndex < lowerBoundIndex then
        Below

    else if pitchIndex == lowerBoundIndex then
        LowerBoundary

    else if pitchIndex > upperBoundIndex then
        Above

    else if pitchIndex == upperBoundIndex then
        UpperBoundary

    else
        Within


intervalPositionWithinVisibleRange : Model -> Interval -> PositionWithinVisibleRange
intervalPositionWithinVisibleRange model interval =
    let
        { start, end } =
            visibleRange model

        lowerBoundIndex =
            Degree.indexOf start

        upperBoundIndex =
            Degree.indexOf end

        intervalFromIndex =
            Degree.indexOf interval.from

        intervalToIndex =
            Degree.indexOf interval.to
    in
    if lowerBoundIndex == intervalFromIndex then
        LowerBoundary

    else if upperBoundIndex == intervalToIndex then
        UpperBoundary

    else if intervalToIndex <= lowerBoundIndex then
        Below

    else if intervalFromIndex >= upperBoundIndex then
        Above

    else
        Within


{-| This feels potentially fragile.

TODO: we'll need some sort of minimum for the portrait to enable scrolling on
small viewports.

-}
scalingFactor : LayoutData -> Int -> Float
scalingFactor layoutData rangeInMoria =
    let
        marginForButton =
            pitchButtonSize layoutData |> toFloat
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


{-| may want padding on this.
-}
view : Model -> Html Msg
view model =
    div
        ([ Attr.id "pitch-space"
         , Styles.transition
         , Attr.attributeIf model.showSpacing Styles.border
         ]
            ++ (case layoutFor model.layout of
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
        -- , class "flex-nowrap"
        [ viewIntervals model
        , viewPitches model
        ]



-- LAYOUT HELPERS


{-| Hypothesis is that this will work for both pitch column and
the interval column. We'll have to see, though.
-}
listAttributes : LayoutData -> List (Html.Attribute Msg)
listAttributes layoutData =
    case layoutFor layoutData of
        Vertical ->
            [ class "flex flex-col-reverse justify-end w-36 mx-4"

            -- , Styles.height (truncate layoutData.viewport.viewport.height - 200)
            ]

        Horizontal ->
            [ Styles.flexRowCentered
            , class "h-24 w-full my-4"
            ]



-- INTERVAL COLUMN


viewIntervals : Model -> Html Msg
viewIntervals model =
    Html.Keyed.ol (listAttributes model.layout)
        (List.map (viewInterval model (visibleRangeInMoria model))
            (Pitch.intervals model.scale)
        )


viewInterval : Model -> Int -> Interval -> ( String, Html Msg )
viewInterval model rangeInMoria interval =
    let
        id =
            "interval-" ++ Degree.toString interval.from ++ "-" ++ Degree.toString interval.to

        position =
            intervalPositionWithinVisibleRange model interval

        layout =
            layoutFor model.layout

        size =
            case position of
                Above ->
                    0

                Below ->
                    0

                _ ->
                    scalingFactor model.layout rangeInMoria
                        -- (case layout of
                        --     Portrait ->
                        --         model.layout.viewport.viewport.height
                        --     Landscape ->
                        --         -- TODO: may want to consider truncating this somehow
                        --         model.layout.viewportOfPitchSpace.viewport.width
                        -- )
                        --     / toFloat rangeInMoria
                        * toFloat interval.moria
                        |> round

        movement =
            Movement.ofInterval model.currentPitch interval

        moria =
            span [ class "text-gray-600" ]
                [ text (String.fromInt interval.moria)
                , viewIf model.showSpacing
                    (text <| " (" ++ String.fromInt size ++ "px)")
                ]

        buttonAttrs =
            [ class "w-full content-center cursor-pointer"
            , classList
                [ ( "bg-slate-200"
                  , shouldHighlightInterval model.currentPitch model.proposedMovement interval
                  )
                , ( "hover:bg-slate-200", Maybe.isJust model.currentPitch )
                ]
            , onFocus (SelectProposedMovement movement)
            , onMouseEnter (SelectProposedMovement movement)
            ]
    in
    ( id
    , li
        [ Attr.id id
        , class <| positionString position
        , Styles.flexRowCentered
        , case layout of
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
                    [ viewIntervalCharacter model.currentPitch degree
                    , moria
                    ]

            DescendTo degree ->
                button
                    (onClick (SelectPitch (Just degree) (Maybe.map AscendTo (Degree.step degree 1)))
                        :: buttonAttrs
                    )
                    [ viewIntervalCharacter model.currentPitch degree
                    , moria
                    ]

            None ->
                div [ class "content-center" ]
                    [ moria ]
          )
            |> viewIf (positionIsVisible position)
        ]
    )


viewIntervalCharacter : Maybe Degree -> Degree -> Html Msg
viewIntervalCharacter currentPitch degree =
    currentPitch
        |> Maybe.map (\current -> Degree.getInterval current degree)
        |> Maybe.andThen IntervalCharacter.basicInterval
        |> Html.Extra.viewMaybe
            (ByzHtmlInterval.view >> List.singleton >> span [ class "me-2" ])


shouldHighlightInterval : Maybe Degree -> Movement -> Interval -> Bool
shouldHighlightInterval currentPitch proposedMovement interval =
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


viewPitches : Model -> Html Msg
viewPitches model =
    Html.Keyed.ol (listAttributes model.layout)
        (List.map (viewPitch model (visibleRangeInMoria model)) Degree.gamutList)


viewPitch : Model -> Int -> Degree -> ( String, Html Msg )
viewPitch model rangeInMoria degree =
    let
        id =
            "pitch-" ++ Degree.toString degree

        layout =
            layoutFor model.layout

        positionWithinRange =
            pitchPositionWithinVisibleRange model degree

        pitch =
            Pitch.pitchPosition model.scale degree

        pitchBelow =
            Degree.step degree -1
                |> Maybe.map (Pitch.pitchPosition model.scale)

        pitchAbove =
            Degree.step degree 1
                |> Maybe.map (Pitch.pitchPosition model.scale)

        scalingFactor_ =
            scalingFactor model.layout rangeInMoria

        size =
            case positionWithinRange of
                Below ->
                    0

                LowerBoundary ->
                    Maybe.map
                        (\above -> toFloat (above - pitch) / 2)
                        pitchAbove
                        |> Maybe.withDefault 0
                        |> (*) scalingFactor_
                        |> round

                Within ->
                    Maybe.map2
                        (\below above -> (toFloat (above - pitch) / 2) + (toFloat (pitch - below) / 2))
                        pitchBelow
                        pitchAbove
                        |> Maybe.withDefault 0
                        |> (*) scalingFactor_
                        |> round

                UpperBoundary ->
                    Maybe.map
                        (\below -> toFloat (pitch - below) / 2)
                        pitchBelow
                        |> Maybe.withDefault 0
                        |> (*) scalingFactor_
                        |> round

                Above ->
                    0

        showSpacingDetails =
            model.showSpacing && positionIsVisible positionWithinRange

        attributeIfVisible =
            Attr.attributeIf (positionIsVisible positionWithinRange)
    in
    ( id
    , li
        [ Attr.id id
        , class <| positionString positionWithinRange
        , Styles.transition
        , Attr.attributeIf showSpacingDetails Styles.border
        , attributeIfVisible Styles.flexCol
        , case layout of
            Vertical ->
                Styles.height size

            Horizontal ->
                Styles.width size
        ]
        [ viewIfLazy (positionIsVisible positionWithinRange)
            (\_ ->
                pitchButton model
                    { degree = degree
                    , pitch = pitch
                    , pitchAbove = pitchAbove
                    , pitchBelow = pitchBelow
                    , positionWithinRange = positionWithinRange
                    , scalingFactor_ = scalingFactor_
                    }
            )
        , viewIf showSpacingDetails
            (text (" (" ++ String.fromInt size ++ "px)"))
        ]
    )


pitchButton :
    Model
    ->
        { degree : Degree
        , pitch : Int
        , pitchAbove : Maybe Int
        , pitchBelow : Maybe Int
        , positionWithinRange : PositionWithinVisibleRange
        , scalingFactor_ : Float
        }
    -> Html Msg
pitchButton model { degree, pitch, pitchAbove, pitchBelow, positionWithinRange, scalingFactor_ } =
    let
        isCurrentPitch =
            Just degree == model.currentPitch

        layout =
            layoutFor model.layout

        position =
            case ( layout, positionWithinRange ) of
                ( _, Below ) ->
                    0

                ( Vertical, LowerBoundary ) ->
                    Maybe.map
                        (\above -> toFloat (above - pitch) / 2)
                        pitchAbove
                        |> Maybe.withDefault 0
                        |> (*) scalingFactor_
                        |> round
                        |> (+) (negate pitchButtonSizeValue)

                ( Horizontal, LowerBoundary ) ->
                    negate pitchButtonSizeValue

                ( Vertical, Within ) ->
                    Maybe.map
                        (\above -> toFloat (above - pitch) / 2)
                        pitchAbove
                        |> Maybe.withDefault 0
                        |> (*) scalingFactor_
                        |> round
                        |> (+) (negate pitchButtonSizeValue)

                ( Horizontal, Within ) ->
                    Maybe.map
                        (\below -> toFloat (pitch - below) / 2)
                        pitchBelow
                        |> Maybe.withDefault 0
                        |> (*) scalingFactor_
                        |> round
                        |> (+) (negate pitchButtonSizeValue)

                ( Vertical, UpperBoundary ) ->
                    negate pitchButtonSizeValue

                ( Horizontal, UpperBoundary ) ->
                    Maybe.map
                        (\below -> toFloat (pitch - below) / 2)
                        pitchBelow
                        |> Maybe.withDefault 0
                        |> (*) scalingFactor_
                        |> round
                        |> (+) (negate pitchButtonSizeValue)

                ( _, Above ) ->
                    0

        pitchButtonSizeValue =
            pitchButtonSize model.layout // 2
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
        , case layoutFor model.layout of
            Vertical ->
                Styles.top position

            Horizontal ->
                Styles.left position
        , classList
            [ ( "bg-red-200", isCurrentPitch )
            , ( "hover:text-green-700 bg-slate-200 hover:bg-slate-300 opacity-75 hover:opacity-100", not isCurrentPitch )
            , ( "text-green-700 bg-slate-300 z-10", Movement.toDegree model.proposedMovement == Just degree )
            ]
        ]
        [ ByzHtmlMartyria.viewWithAttributes
            [ Styles.left -3, Styles.top -3 ]
            (Martyria.for model.scale degree)
        ]


{-| 64px default, 48px below the sm breakpoint.
-}
pitchButtonSizeClass : Html.Attribute msg
pitchButtonSizeClass =
    class "w-12 h-12 sm:w-16 sm:h-16 text-xl sm:text-3xl"


pitchButtonSize : LayoutData -> Int
pitchButtonSize layoutData =
    if layoutData.viewport.viewport.width < 640 then
        48

    else
        64
