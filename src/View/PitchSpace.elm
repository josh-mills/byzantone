module View.PitchSpace exposing (view)

{-| View logic for pitch space (i.e., the intervalic space and positioned pitches)
-}

import Byzantine.Accidental as Accidental
import Byzantine.ByzHtml.Accidental as ByzHtmlAccidental
import Byzantine.ByzHtml.Interval as ByzHtmlInterval
import Byzantine.ByzHtml.Martyria as ByzHtmlMartyria
import Byzantine.Degree as Degree exposing (Degree)
import Byzantine.DetectedPitch exposing (DetectedPitch)
import Byzantine.Frequency as Frequency
import Byzantine.Interval as Interval exposing (Interval)
import Byzantine.IntervalCharacter as IntervalCharacter
import Byzantine.Martyria as Martyria
import Byzantine.Pitch as Pitch exposing (Pitch, PitchString)
import Byzantine.PitchPosition as PitchPosition
import Html exposing (Html, button, div, li, span, text)
import Html.Attributes as Attr exposing (class, classList)
import Html.Attributes.Extra as Attr exposing (attributeMaybe)
import Html.Events exposing (onClick, onFocus, onMouseEnter, onMouseLeave)
import Html.Extra exposing (viewIf, viewIfLazy, viewMaybe)
import Html.Lazy
import Maybe.Extra as Maybe
import Model.AudioSettings as AudioSettings exposing (AudioSettings, Responsiveness(..))
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
        )
import Model.PitchState as PitchState exposing (PitchState, ProposedAccidental(..))
import Movement exposing (Movement(..))
import Result.Extra
import Round
import Styles
import Time
import Update exposing (Msg(..))



-- WRAPPER AND VIEW HELPERS


view : PitchSpaceData -> AudioSettings -> ModeSettings -> PitchState -> Maybe { detectedPitch : DetectedPitch, timestamp : Time.Posix } -> Html Msg
view pitchSpaceData audioSettings modeSettings pitchState maybeTimestampedDetectedPitch =
    div
        ([ Attr.id "pitch-space"
         , Styles.transition
         , Attr.attributeIf LayoutData.showSpacing Styles.border
         ]
            ++ (if PitchSpaceData.isVertical pitchSpaceData.display then
                    [ Styles.flexRow
                    , class "justify-around my-8 md:me-12 pb-12"
                    ]

                else
                    [ Styles.flexCol, class "mx-8 mb-8" ]
               )
        )
        [ Html.Lazy.lazy3 viewIntervals pitchSpaceData modeSettings pitchState
        , Html.Lazy.lazy3 viewPitchTracker
            pitchSpaceData
            audioSettings
            (Maybe.map .detectedPitch maybeTimestampedDetectedPitch)
        , Html.Lazy.lazy3 viewPitches pitchSpaceData modeSettings pitchState
        , Html.Lazy.lazy2 viewAccidentalButtons pitchSpaceData.display pitchState.proposedAccidental
        ]


listAttributes : Display -> List (Html.Attribute Msg)
listAttributes display =
    if PitchSpaceData.isVertical display then
        [ class "flex flex-col-reverse justify-end mx-4 md:mx-8" ]

    else
        [ Styles.flexRowCentered
        , class "w-full my-4"
        ]


{-| 64px default, 48px below the sm breakpoint.
-}
pitchButtonSizeClass : Html.Attribute msg
pitchButtonSizeClass =
    class "w-12 h-12 sm:w-16 sm:h-16 text-xl sm:text-3xl"



-- INTERVAL COLUMN


viewIntervals : PitchSpaceData -> ModeSettings -> PitchState -> Html Msg
viewIntervals pitchSpaceData modeSettings pitchState =
    Html.ol
        (onMouseLeave (SelectProposedMovement None)
            :: (if PitchSpaceData.isVertical pitchSpaceData.display then
                    class "w-24 md:w-36"

                else
                    class "h-16 md:h-24"
               )
            :: listAttributes pitchSpaceData.display
        )
        (List.map (viewInterval pitchSpaceData modeSettings pitchState)
            (PitchSpaceData.intervalsWithVisibility modeSettings.scale pitchSpaceData pitchState.proposedMovement)
        )


viewInterval : PitchSpaceData -> ModeSettings -> PitchState -> ( Interval, PositionWithinVisibleRange ) -> Html Msg
viewInterval pitchSpaceData modeSettings pitchState ( interval, position ) =
    let
        currentPitch =
            PitchState.currentPitch modeSettings.scale pitchState

        currentPitchString =
            Maybe.unwrap "" (Pitch.encode modeSettings.scale) currentPitch

        shouldHighlight =
            shouldHighlightInterval currentPitch pitchState.proposedMovement interval
    in
    Html.Lazy.lazy6 viewIntervalLazy
        currentPitchString
        pitchSpaceData.display
        pitchSpaceData.scalingFactor
        (Interval.encode modeSettings.scale interval)
        position
        shouldHighlight


viewIntervalLazy :
    PitchString
    -> Display
    -> Float
    -> String
    -> PositionWithinVisibleRange
    -> Bool
    -> Html Msg
viewIntervalLazy currentPitchString display scalingFactor intervalString position shouldHighlight =
    let
        interval =
            Interval.decode intervalString

        intervalMoria =
            Result.Extra.unwrap -1 Interval.unwrapSize interval

        intervalFromDegree =
            Result.Extra.unwrap "err"
                (\{ from } -> Pitch.unwrapDegree from |> Degree.toString)
                interval

        intervalToDegree =
            Result.Extra.unwrap "err"
                (\{ to } -> Pitch.unwrapDegree to |> Degree.toString)
                interval

        size =
            case position of
                Above ->
                    0

                Below ->
                    0

                _ ->
                    toFloat intervalMoria * scalingFactor

        currentPitch =
            Pitch.decode currentPitchString
                |> Result.Extra.unwrap Nothing
                    (Just << Tuple.second)

        movement =
            Result.Extra.unwrap Movement.None (Movement.ofInterval currentPitch) interval

        buttonAttrs =
            [ class "w-full content-center cursor-pointer"
            , classList
                [ ( "bg-slate-200", shouldHighlight )
                , ( "hover:bg-slate-200", Maybe.isJust currentPitch )
                ]
            , onFocus (SelectProposedMovement movement)
            , onMouseEnter (SelectProposedMovement movement)
            ]

        moriaDomNode =
            Html.Lazy.lazy2 viewMoria size intervalMoria
    in
    li
        (Attr.id ("interval-" ++ intervalFromDegree ++ "-" ++ intervalToDegree)
            :: (if PitchSpaceData.isVertical display then
                    [ Styles.height size
                    , class "border-r-0"
                    ]

                else
                    [ Styles.width size
                    , class "border-b-0"
                    ]
               )
            ++ [ Styles.flexRowCentered
               , Styles.transition
               , Attr.attributeIf (PitchSpaceData.positionIsVisible position) Styles.border
               ]
        )
        [ viewIfLazy (PitchSpaceData.positionIsVisible position)
            (\_ ->
                case movement of
                    AscendTo toPitch ->
                        button
                            {- (Maybe.map DescendTo (Degree.step pitch -1)) ??? -}
                            (onClick (SelectPitch (Just toPitch) Nothing)
                                :: buttonAttrs
                            )
                            [ viewIntervalCharacter currentPitch toPitch
                            , moriaDomNode
                            ]

                    DescendTo toPitch ->
                        button
                            {- (Maybe.map AscendTo (Degree.step pitch 1)) ??? -}
                            (onClick (SelectPitch (Just toPitch) Nothing)
                                :: buttonAttrs
                            )
                            [ viewIntervalCharacter currentPitch toPitch
                            , moriaDomNode
                            ]

                    None ->
                        div [ class "content-center" ]
                            [ moriaDomNode ]
            )
        ]


{-| TODO: We should have this take pitches, not just degrees. And probably don't
wrap in a maybe.
-}
viewIntervalCharacter : Maybe Pitch -> Pitch -> Html Msg
viewIntervalCharacter maybeFromPitch toPitch =
    let
        toAccidentalStr =
            Pitch.unwrapAccidental toPitch
                |> Maybe.unwrap "" Accidental.toString
    in
    maybeFromPitch
        |> Maybe.map
            (\fromPitch ->
                Degree.getInterval
                    (Pitch.unwrapDegree fromPitch)
                    (Pitch.unwrapDegree toPitch)
            )
        |> viewMaybe (Html.Lazy.lazy2 viewIntervalCharacterLazy toAccidentalStr)


viewIntervalCharacterLazy : String -> Int -> Html Msg
viewIntervalCharacterLazy toAccidentalStr interval =
    let
        toAccidental =
            Accidental.fromString toAccidentalStr |> Result.toMaybe
    in
    IntervalCharacter.basicInterval interval
        |> Maybe.map (IntervalCharacter.applyAccidental toAccidental)
        |> Html.Extra.viewMaybe
            (\intervalCharacter ->
                span [ class "me-2" ] [ ByzHtmlInterval.view intervalCharacter ]
            )


viewMoria : Float -> Int -> Html msg
viewMoria size moria =
    span [ class "text-gray-600" ]
        [ text (String.fromInt moria)
        , viewIfLazy LayoutData.showSpacing
            (\_ -> text <| " (" ++ Round.round 2 size ++ "px)")
        ]


shouldHighlightInterval : Maybe Pitch -> Movement -> Interval -> Bool
shouldHighlightInterval currentPitch proposedMovement interval =
    Maybe.unwrap False
        (\current ->
            let
                currentPitchIndex =
                    Degree.indexOf current
            in
            case proposedMovement of
                AscendTo degree ->
                    let
                        toIndex =
                            Degree.indexOf (Pitch.unwrapDegree interval.to)
                    in
                    (currentPitchIndex < toIndex)
                        && (toIndex <= Degree.indexOf (Pitch.unwrapDegree degree))

                DescendTo degree ->
                    let
                        fromIndex =
                            Degree.indexOf (Pitch.unwrapDegree interval.from)
                    in
                    (currentPitchIndex > fromIndex)
                        && (fromIndex >= Degree.indexOf (Pitch.unwrapDegree degree))

                None ->
                    False
        )
        (Maybe.map Pitch.unwrapDegree currentPitch)



-- PITCH TRACKER COLUMN


viewPitchTracker : PitchSpaceData -> AudioSettings -> Maybe DetectedPitch -> Html Msg
viewPitchTracker pitchSpaceData audioSettings detectedPitch =
    div
        (if PitchSpaceData.isVertical pitchSpaceData.display then
            [ Styles.flexCol
            , Styles.transition
            , classList
                [ ( "w-6 md:w-12", audioSettings.audioMode == AudioSettings.Listen )
                , ( "w-0", audioSettings.audioMode == AudioSettings.Play )
                ]
            ]

         else
            [ Styles.flexRow
            , Styles.transition
            , classList
                [ ( "h-6 w-full", audioSettings.audioMode == AudioSettings.Listen )
                , ( "h-0", audioSettings.audioMode == AudioSettings.Play )
                ]
            ]
        )
        [ viewMaybe (viewPitchIndicator pitchSpaceData audioSettings) detectedPitch
        , viewMaybe (viewDetectedPitch pitchSpaceData audioSettings) detectedPitch
        ]


viewDetectedPitch : PitchSpaceData -> AudioSettings -> DetectedPitch -> Html Msg
viewDetectedPitch pitchSpaceData audioSettings detectedPitch =
    div
        [ class "w-full h-full font-mono text-center sm:text-lg lg:text-xl z-10 flex items-center"
        , classList
            [ ( "justify-center", not (PitchSpaceData.isVertical pitchSpaceData.display) )
            ]
        ]
        [ text (formatPitchFeedback audioSettings.pitchFeedback detectedPitch)
        ]


formatPitchFeedback : AudioSettings.PitchFeedbackUnit -> DetectedPitch -> String
formatPitchFeedback pitchFeedback detectedPitch =
    case pitchFeedback of
        AudioSettings.Hz ->
            Frequency.displayString detectedPitch.frequency

        AudioSettings.Cents ->
            let
                cents =
                    detectedPitch.offset * 1200 / 72

                sign =
                    if cents >= 0 then
                        "+"

                    else
                        ""
            in
            sign ++ String.fromInt (round cents) ++ " cents"

        AudioSettings.Moria ->
            let
                moria =
                    detectedPitch.offset

                sign =
                    if moria >= 0 then
                        "+"

                    else
                        ""
            in
            sign ++ Round.round 1 moria ++ " moria"


viewPitchIndicator : PitchSpaceData -> AudioSettings -> DetectedPitch -> Html Msg
viewPitchIndicator pitchSpaceData { responsiveness } detectedPitch =
    let
        position =
            case PitchSpaceData.displayToLayout pitchSpaceData.display of
                Vertical ->
                    let
                        endPosition =
                            PitchPosition.toFloat pitchSpaceData.visibleRange.endPosition
                    in
                    Styles.top
                        ((endPosition - detectedPitch.pitchPosition)
                            * pitchSpaceData.scalingFactor
                            - PitchSpaceData.pitchIndicatorPositionAdjustment pitchSpaceData.display
                        )

                Horizontal ->
                    let
                        startPosition =
                            PitchPosition.toFloat pitchSpaceData.visibleRange.startPosition
                    in
                    Styles.left
                        ((detectedPitch.pitchPosition - startPosition)
                            * pitchSpaceData.scalingFactor
                            + PitchSpaceData.pitchIndicatorPositionAdjustment pitchSpaceData.display
                        )

        absOffset =
            abs detectedPitch.offset

        color =
            if absOffset < 0.6 then
                "bg-green-500"

            else if absOffset <= 1.2 then
                "bg-amber-400"

            else
                "bg-red-500"
    in
    div
        [ class "relative h-6 w-6 rounded-full"
        , position
        , Attr.style "transition" <|
            case responsiveness of
                Sensitive ->
                    "background-color 80ms ease-in-out, left 80ms ease-out, top 80ms ease-out"

                Smooth ->
                    "background-color 150ms ease-in-out, left 150ms ease-out, top 150ms ease-out"
        , class color
        ]
        []



-- PITCH COLUMN


viewPitches : PitchSpaceData -> ModeSettings -> PitchState -> Html Msg
viewPitches pitchSpaceData modeSettings pitchState =
    let
        currentPitch =
            PitchState.currentPitch modeSettings.scale pitchState
    in
    Html.ol (listAttributes pitchSpaceData.display)
        (List.map
            (viewPitch pitchSpaceData modeSettings pitchState currentPitch)
            Degree.gamutList
        )


{-| Renders a single pitch element in the pitch space.
-}
viewPitch : PitchSpaceData -> ModeSettings -> PitchState -> Maybe Pitch -> Degree -> Html Msg
viewPitch pitchSpaceData modeSettings pitchState currentPitch degree =
    let
        pitchString =
            DegreeDataDict.get degree pitchSpaceData.pitches
                |> Pitch.encode modeSettings.scale

        isCurrentDegree =
            Just degree == Maybe.map Pitch.unwrapDegree currentPitch

        isonStatusIndicator =
            DegreeDataDict.get degree pitchSpaceData.isonIndicators

        pitchPositions =
            PitchSpaceData.encodePitchPositionContext pitchSpaceData degree

        shouldHighlight =
            PitchSpaceData.canBeSelectedAsIson isonStatusIndicator
                || (case pitchState.proposedAccidental of
                        Apply accidental ->
                            PitchPosition.isValidInflection modeSettings.scale accidental degree

                        CancelAccidental ->
                            DegreeDataDict.get degree pitchState.appliedAccidentals
                                |> Maybe.isJust

                        NoProposedAccidental ->
                            False
                   )

        positionWithinRange =
            DegreeDataDict.get degree pitchSpaceData.pitchVisibility
    in
    Html.Lazy.lazy8 viewPitchLazy
        pitchString
        isCurrentDegree
        pitchSpaceData.display
        isonStatusIndicator
        pitchPositions
        shouldHighlight
        positionWithinRange
        pitchSpaceData.scalingFactor


viewPitchLazy :
    PitchString
    -> Bool
    -> Display
    -> IsonSelectionIndicator
    -> PitchPositionContextString
    -> Bool
    -> PositionWithinVisibleRange
    -> Float
    -> Html Msg
viewPitchLazy pitchString isCurrentDegree display isonStatusIndicator pitchPositions shouldHighlight positionWithinRange scalingFactor =
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
                    pitchPositions
                    shouldHighlight
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

Since lazy view is covered at a higher level, we shouldn't need to re-decode the
pitch string.

-}
pitchButton :
    PitchString
    -> Bool
    -> Display
    -> PitchPositionContextString
    -> Bool
    -> PositionWithinVisibleRange
    -> Float
    -> Html Msg
pitchButton pitchString isCurrentDegree display pitchPositions shouldHighlight positionWithinRange scalingFactor =
    let
        ( decodedScale, decodedPitch, decodedDegree ) =
            Result.Extra.unwrap ( Nothing, Nothing, Nothing )
                (\( s, p ) -> ( Just s, Just p, Just (Pitch.unwrapDegree p) ))
                (Pitch.decode pitchString)

        position =
            pitchElementPosition PitchButton
                display
                pitchPositions
                positionWithinRange
                scalingFactor
    in
    button
        [ attributeMaybe (\degree -> onClick (PitchButtonClicked degree)) decodedDegree
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
                    [ ByzHtmlAccidental.view ByzHtmlAccidental.Red accidental ]
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



-- ACCIDENTAL BUTTONS


viewAccidentalButtons : Display -> ProposedAccidental -> Html Msg
viewAccidentalButtons display proposedAccidental =
    div
        (if PitchSpaceData.isVertical display then
            [ Styles.flexCol, class "justify-center me-4" ]

         else
            [ Styles.flexRow, class "justify-center me-4" ]
        )
        [ Html.fieldset
            [ Styles.borderRounded
            , if PitchSpaceData.isVertical display then
                class "flex flex-col-reverse flex-wrap content-center"

              else
                Styles.flexRow
            , class "justify-center"
            , class "px-2 pb-1 mb-2 gap-1"
            ]
            (Html.legend [ class "px-1" ] [ Html.text "Accidental" ]
                :: List.concat
                    [ Accidental.allFlats
                        |> List.map (viewAccidentalButton proposedAccidental << Apply)
                    , [ viewAccidentalButton proposedAccidental CancelAccidental ]
                    , Accidental.allSharps
                        |> List.map (viewAccidentalButton proposedAccidental << Apply)
                    ]
            )
        ]


viewAccidentalButton : ProposedAccidental -> ProposedAccidental -> Html Msg
viewAccidentalButton currentProposedAccidental buttonProposedAccidental =
    let
        isCurrent =
            currentProposedAccidental == buttonProposedAccidental

        buttonContent =
            case buttonProposedAccidental of
                Apply accidental ->
                    ByzHtmlAccidental.view ByzHtmlAccidental.InheritColor accidental

                CancelAccidental ->
                    Html.text "×"

                NoProposedAccidental ->
                    Html.Extra.nothing
    in
    button
        [ Styles.buttonClass
        , Styles.transition
        , class "text-2xl sm:text-3xl min-w-8 sm:min-w-12 max-w-14 m-1"
        , classList
            [ ( "text-blue-700 border-2 border-blue-700", isCurrent )
            , ( "border-2 border-transparent", not isCurrent )
            ]
        , onClick
            (if isCurrent then
                SelectProposedAccidental NoProposedAccidental

             else
                SelectProposedAccidental buttonProposedAccidental
            )
        ]
        [ buttonContent ]
