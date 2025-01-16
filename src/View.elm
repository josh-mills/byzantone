module View exposing (view)

import Browser.Dom as Dom
import Byzantine.ByzHtml.Interval as Interval
import Byzantine.ByzHtml.Martyria as Martyria
import Byzantine.Degree as Degree exposing (Degree(..))
import Byzantine.IntervalCharacter exposing (..)
import Byzantine.Martyria as Martyria
import Byzantine.Pitch as Pitch exposing (Interval)
import Byzantine.Scale as Scale exposing (Scale(..))
import Html exposing (Html, button, div, fieldset, h1, input, label, legend, main_, p, span, text)
import Html.Attributes as Attr exposing (checked, class, classList, for, id, style, type_)
import Html.Attributes.Extra as Attr
import Html.Events exposing (onClick, onFocus, onInput, onMouseEnter, onMouseLeave)
import Html.Extra exposing (viewIf, viewMaybe)
import Icons
import Json.Decode exposing (Decoder)
import List.Extra as List
import Maybe.Extra as Maybe
import Model exposing (AudioSettings, Model)
import Movement exposing (Movement(..))
import Update exposing (Msg(..))



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "lg:container m-4 lg:mx-auto" ]
        [ audio model
        , header
        , main_
            [ class "flex flex-row font-serif"
            , Html.Events.on "keydown" keyDecoder
            ]
            [ pitchSpace model
            , viewControls model
            ]
        ]


audio : Model -> Html msg
audio model =
    Html.node "chant-engine"
        [ model.audioSettings.gain
            |> String.fromFloat
            |> Attr.attribute "gain"
        , case model.currentPitch of
            Nothing ->
                Attr.empty

            Just pitch ->
                Pitch.frequency model.scale pitch
                    |> String.fromFloat
                    |> Attr.attribute "ison"
        ]
        []


header : Html Msg
header =
    div [ class "flex flex-col mb-4" ]
        [ h1 [ class "font-heading text-4xl text-center" ]
            [ text "ByzanTone" ]
        , p [ class "font-serif text-center" ]
            [ text "A tool for learning the pitches and intervals of Byzantine chant." ]
        ]


pitchSpace : Model -> Html Msg
pitchSpace model =
    let
        intervals =
            Pitch.intervalsFrom model.scale Ni Pa_
    in
    div
        [ class "flex flex-row flex-nowrap transition-all duration-500"
        , class "min-w-[360px]"
        ]
        [ intervalCol model intervals
        , pitchCol model intervals
        ]


intervalCol : Model -> List Interval -> Html Msg
intervalCol ({ showSpacing, viewport } as model) intervals =
    let
        definedIntervals =
            intervals
                |> List.reverse
                |> List.map (viewInterval model)

        spacerTop =
            List.last intervals
                |> viewMaybe (spacerInterval viewport showSpacing)

        spacerBottom =
            List.head intervals
                |> viewMaybe (spacerInterval viewport showSpacing)
                |> List.singleton
    in
    div
        [ class "w-36"
        , onMouseLeave (SelectProposedMovement None)
        ]
        (spacerTop :: definedIntervals ++ spacerBottom)


viewInterval : Model -> Interval -> Html Msg
viewInterval { currentPitch, proposedMovement, viewport } interval =
    let
        viewIntervalCharacter degree =
            currentPitch
                |> Maybe.map (\current -> Degree.getInterval current degree)
                |> Maybe.andThen basicInterval
                |> Html.Extra.viewMaybe
                    (Interval.view >> List.singleton >> span [ class "me-2" ])

        ( maybeIntervalCharacter, attrs, element ) =
            let
                movementOfThisInterval =
                    Movement.ofInterval currentPitch interval

                movementTo degree maybeNewMovement =
                    ( viewIntervalCharacter degree
                    , [ onClick (SelectPitch (Just degree) maybeNewMovement)

                      -- TODO: onBlur or onMouseLeave? how to handle this?
                      , onFocus (SelectProposedMovement movementOfThisInterval)
                      , onMouseEnter (SelectProposedMovement movementOfThisInterval)
                      ]
                    , button
                    )
            in
            case movementOfThisInterval of
                AscendTo degree ->
                    let
                        newMovement : Maybe Movement
                        newMovement =
                            Maybe.map DescendTo (Degree.step degree -1)
                    in
                    movementTo degree newMovement

                DescendTo degree ->
                    let
                        newMovement : Maybe Movement
                        newMovement =
                            Maybe.map AscendTo (Degree.step degree 1)
                    in
                    movementTo degree newMovement

                None ->
                    ( Html.Extra.nothing, [], div )
    in
    element
        ([ class "border border-gray-300"
         , class "flex flex-row justify-center w-full"
         , height (interval.moria * heightFactor viewport)
         , transition
         , classList
            [ ( "bg-slate-200"
              , shouldHighlight currentPitch proposedMovement interval
              )
            , ( "hover:bg-slate-200", Maybe.isJust currentPitch )
            ]
         ]
            ++ attrs
        )
        [ span [ class "my-auto" ]
            [ maybeIntervalCharacter
            , text (String.fromInt interval.moria)
            ]
        ]


spacerInterval : Dom.Viewport -> Bool -> Interval -> Html Msg
spacerInterval viewport showSpacing { moria } =
    div
        [ height (moria * heightFactor viewport // 2)
        , transition
        , classList [ ( "text-center bg-slate-300", showSpacing ) ]
        ]
        [ viewIf showSpacing <| text <| "(" ++ String.fromInt (moria // 2) ++ ")" ]


pitchCol : Model -> List Interval -> Html Msg
pitchCol model intervals =
    intervalsToPitchHeights intervals
        |> List.map (viewPitch model)
        |> List.reverse
        |> div [ class "w-16" ]


viewPitch : Model -> PitchHeight -> Html Msg
viewPitch { scale, showSpacing, currentPitch, proposedMovement, viewport } pitchHeight =
    let
        degree =
            Just pitchHeight.degree

        isCurrentPitch =
            degree == currentPitch

        totalHeight =
            pitchHeight.aboveCenter + pitchHeight.belowCenter |> String.fromInt

        spacingText =
            -- for dev purposes only; will eventually be deleted
            "(" ++ totalHeight ++ " = " ++ String.fromInt pitchHeight.belowCenter ++ " + " ++ String.fromInt pitchHeight.aboveCenter ++ ")"
    in
    div
        [ classList [ ( "border border-gray-500 bg-slate-200", showSpacing ) ]
        , class "flex flex-row justify-center"
        , height <| (pitchHeight.belowCenter + pitchHeight.aboveCenter) * heightFactor viewport
        , transition
        ]
        [ button
            [ class "flex px-4 w-full rounded-full"
            , id <| "p_" ++ Degree.toString pitchHeight.degree
            , transition
            , classList
                [ ( "bg-green-300", showSpacing ) -- for dev purposes only; will eventually be deleted
                , ( "text-green-700 bg-slate-200", Movement.toDegree proposedMovement == degree )
                , ( "bg-red-200", isCurrentPitch )
                , ( "hover:text-green-700 hover:bg-slate-200", not isCurrentPitch )
                ]
            , onClick <|
                if isCurrentPitch then
                    SelectPitch Nothing Nothing

                else
                    SelectPitch degree Nothing
            ]
            [ span
                [ style "padding-top" <| String.fromInt (pitchHeight.aboveCenter * (heightFactor viewport - 2)) ++ "px"
                , transition
                , class "flex flex-row gap-2 absolute"
                , classList [ ( "text-red-600", isCurrentPitch ) ]
                ]
                [ div [ class "text-xl sm:text-3xl relative -top-5" ]
                    [ viewMaybe (Martyria.view << Martyria.for scale) degree ]
                , viewIf isCurrentPitch <|
                    div [ class "w-4 ms-2" ] [ Icons.xmark ]
                , viewIf showSpacing <| span [ class "ms-2" ] [ text spacingText ]
                ]
            ]
        ]



-- CONTROLS


viewControls : Model -> Html Msg
viewControls model =
    div []
        [ spacingButton model.showSpacing
        , selectScale model
        , viewCurrentPitch model.currentPitch
        , clearPitchButton
        , viewProposedMovement model.proposedMovement
        , gainInput model.audioSettings
        ]


viewProposedMovement : Movement -> Html Msg
viewProposedMovement movement_ =
    div []
        [ text <| "Proposed Movement: "
        , case movement_ of
            AscendTo degree ->
                text <| "ascend to " ++ Degree.toString degree

            DescendTo degree ->
                text <| "descend to " ++ Degree.toString degree

            None ->
                text "none"
        ]


spacingButton : Bool -> Html Msg
spacingButton showSpacing =
    button
        [ buttonClass
        , onClick ToggleSpacing
        ]
        [ text <|
            if showSpacing then
                "hide spacing"

            else
                "show spacing"
        ]


selectScale : Model -> Html Msg
selectScale model =
    let
        radioOption scale =
            let
                scaleName =
                    Scale.name scale
            in
            div []
                [ input
                    [ type_ "radio"
                    , onClick (SetScale scale)
                    , class "cursor-pointer m-2"
                    , id <| "select" ++ scaleName
                    , checked (model.scale == scale)
                    ]
                    []
                , label
                    [ for <| "select" ++ scaleName
                    , class "cursor-pointer"
                    ]
                    [ text scaleName ]
                ]
    in
    fieldset [] <|
        legend [] [ text "Select scale" ]
            :: List.map radioOption Scale.all


viewCurrentPitch : Maybe Degree -> Html Msg
viewCurrentPitch pitch =
    div [ class "mt-2" ]
        [ text <| "Current Pitch: "
        , case pitch of
            Nothing ->
                text "none"

            Just p ->
                Degree.text p
        ]


clearPitchButton : Html Msg
clearPitchButton =
    button
        [ buttonClass
        , onClick (SelectPitch Nothing Nothing)
        ]
        [ text "clear" ]


gainInput : AudioSettings -> Html Msg
gainInput { gain } =
    let
        ( buttonText, msg ) =
            if gain > 0 then
                ( "mute", SetGain 0 )

            else
                ( "unmute", SetGain 0.2 )
    in
    div []
        [ button
            [ buttonClass
            , class "w-24 mr-4"
            , onClick msg
            ]
            [ text buttonText ]
        , input
            [ type_ "range"
            , Attr.min "0"
            , Attr.max "1"
            , Attr.step "0.02"
            , Attr.value <| String.fromFloat gain
            , onInput (SetGain << Maybe.withDefault gain << String.toFloat)
            ]
            []
        ]



-- HELPERS


buttonClass : Html.Attribute Msg
buttonClass =
    class "bg-gray-200 my-2 py-1 px-3 rounded-md"


{-| in px
-}
height : Int -> Html.Attribute Msg
height h =
    style "height" (String.fromInt h ++ "px")


heightFactor : Dom.Viewport -> Int
heightFactor viewport =
    (viewport.viewport.height / 100)
        |> truncate
        |> clamp 6 12


transition : Html.Attribute Msg
transition =
    class "transition-all duration-500"


shouldHighlight : Maybe Degree -> Movement -> Interval -> Bool
shouldHighlight currentPitch proposedMovement interval =
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


keyDecoder : Decoder Msg
keyDecoder =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.map Keydown



-- TYPES


{-| These are fundamentally view concerns, not domain types.
-}
type alias PitchHeight =
    { degree : Degree
    , belowCenter : Int
    , aboveCenter : Int
    }


intervalsToPitchHeights : List Interval -> List PitchHeight
intervalsToPitchHeights intervals =
    let
        go : PitchHeight -> List Interval -> List PitchHeight
        go prev rest =
            case rest of
                [] ->
                    []

                x :: [] ->
                    [ { degree = x.from
                      , belowCenter = prev.aboveCenter
                      , aboveCenter = x.moria // 2
                      }
                    , { degree = x.to
                      , belowCenter = x.moria // 2
                      , aboveCenter = x.moria // 2
                      }
                    ]

                x :: xs ->
                    let
                        next =
                            { degree = x.from
                            , belowCenter = prev.aboveCenter
                            , aboveCenter = x.moria // 2
                            }
                    in
                    next :: go next xs
    in
    case intervals of
        [] ->
            []

        x :: xs ->
            let
                firstPitch =
                    { degree = x.from
                    , belowCenter = x.moria // 2
                    , aboveCenter = x.moria // 2
                    }
            in
            firstPitch :: go firstPitch xs
