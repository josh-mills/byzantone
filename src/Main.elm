module Main exposing (main)

import Browser
import Byzantine.ByzHtml.Interval as Interval
import Byzantine.ByzHtml.Martyria as Martyria
import Byzantine.Degree as Degree exposing (Degree(..))
import Byzantine.IntervalCharacter exposing (..)
import Byzantine.Martyria as Martyria
import Byzantine.Pitch as Pitch exposing (Interval)
import Byzantine.Scale as Scale exposing (Scale(..))
import Html exposing (Html, button, div, fieldset, input, label, legend, main_, span, text)
import Html.Attributes exposing (checked, class, classList, for, id, style, type_)
import Html.Events exposing (onClick, onFocus, onMouseEnter)
import Html.Extra exposing (viewIf, viewMaybe)
import Icons
import List.Extra as List
import Maybe.Extra as Maybe



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { scale = Diatonic
      , showSpacing = False
      , currentPitch = Nothing
      , proposedMovement = None
      }
    , Cmd.none
    )



-- MODEL


type alias Model =
    { scale : Scale
    , showSpacing : Bool
    , currentPitch : Maybe Degree
    , proposedMovement : Movement
    }



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


type Movement
    = AscendTo Degree
    | DescendTo Degree
    | None


movement : Maybe Degree -> Interval -> Movement
movement currentPitch interval =
    case currentPitch of
        Nothing ->
            None

        Just current ->
            if Degree.indexOf interval.to > Degree.indexOf current then
                AscendTo interval.to

            else if Degree.indexOf interval.from < Degree.indexOf current then
                DescendTo interval.from

            else
                None


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



-- UPDATE


type Msg
    = SelectPitch (Maybe Degree)
    | SelectProposedMovement Movement
    | SetScale Scale
    | ToggleSpacing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectPitch pitch ->
            ( { model
                | currentPitch = pitch
                , proposedMovement = None
              }
            , Cmd.none
            )

        SelectProposedMovement movement_ ->
            ( { model | proposedMovement = movement_ }
            , Cmd.none
            )

        SetScale scale ->
            ( { model
                | scale = scale

                -- , currentPitch = Nothing -- consider this.
              }
            , Cmd.none
            )

        ToggleSpacing ->
            ( { model | showSpacing = not model.showSpacing }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    main_ [ class "container m-4 flex flex-row" ]
        [ pitchSpace model
        , viewControls model
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
intervalCol ({ showSpacing } as model) intervals =
    let
        definedIntervals =
            intervals
                |> List.reverse
                |> List.map (viewInterval model)

        spacerTop =
            List.last intervals
                |> viewMaybe (spacerInterval showSpacing)

        spacerBottom =
            List.head intervals
                |> viewMaybe (spacerInterval showSpacing)
                |> List.singleton
    in
    div [ class "w-36" ]
        (spacerTop :: definedIntervals ++ spacerBottom)


viewInterval : Model -> Interval -> Html Msg
viewInterval { currentPitch, proposedMovement } interval =
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
                    movement currentPitch interval

                movementTo degree =
                    ( viewIntervalCharacter degree
                    , [ onClick (SelectPitch (Just degree))

                      -- TODO: onBlur or onMouseLeave? how to handle this?
                      , onFocus (SelectProposedMovement movementOfThisInterval)
                      , onMouseEnter (SelectProposedMovement movementOfThisInterval)
                      ]
                    , button
                    )
            in
            case movementOfThisInterval of
                AscendTo degree ->
                    movementTo degree

                DescendTo degree ->
                    movementTo degree

                None ->
                    ( Html.Extra.nothing, [], div )
    in
    element
        ([ class "border border-gray-300"
         , class "flex flex-row justify-center w-full"
         , height (interval.moria * 10)
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


spacerInterval : Bool -> Interval -> Html Msg
spacerInterval showSpacing { moria } =
    div
        [ height (moria * 10 // 2)
        , transition
        , classList [ ( "text-center bg-slate-300", showSpacing ) ]
        ]
        [ viewIf showSpacing <| text <| "(" ++ String.fromInt (moria // 2) ++ ")" ]


pitchCol : Model -> List Interval -> Html Msg
pitchCol model intervals =
    intervalsToPitchHeights intervals
        |> List.map (viewPitch model)
        |> List.reverse
        |> div []


viewPitch : Model -> PitchHeight -> Html Msg
viewPitch { scale, showSpacing, currentPitch } pitchHeight =
    let
        degree =
            Just pitchHeight.degree

        isCurrentPitch =
            degree == currentPitch

        totalHeight =
            pitchHeight.aboveCenter + pitchHeight.belowCenter |> String.fromInt

        spacingText =
            "(" ++ totalHeight ++ " = " ++ String.fromInt pitchHeight.belowCenter ++ " + " ++ String.fromInt pitchHeight.aboveCenter ++ ")"
    in
    div
        [ classList [ ( "border border-gray-500 bg-slate-200", showSpacing ) ]
        , class "flex flex-row justify-center"
        , height <| (pitchHeight.belowCenter + pitchHeight.aboveCenter) * 10
        , transition
        ]
        [ button
            [ class "flex px-4 w-full min-w-[40px] hover:text-green-700"
            , classList [ ( "bg-green-300", showSpacing ) ]
            , onClick <|
                if isCurrentPitch then
                    SelectPitch Nothing

                else
                    SelectPitch degree
            ]
            [ span
                [ style "padding-top" <| String.fromInt (pitchHeight.aboveCenter * 8) ++ "px"
                , transition
                , class "flex flex-row gap-2 absolute"
                , classList [ ( "text-red-600", isCurrentPitch ) ]
                ]
                [ div [ class "text-3xl relative -top-5" ]
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
        [ class "bg-gray-100 m-2 p-2 rounded-md"
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
                text <| Degree.toString p
        ]


clearPitchButton : Html Msg
clearPitchButton =
    button
        [ class "bg-gray-200 my-2 py-1 px-3 rounded-md"
        , onClick (SelectPitch Nothing)
        ]
        [ text "clear" ]



-- HELPERS


{-| in px
-}
height : Int -> Html.Attribute Msg
height h =
    style "height" (String.fromInt h ++ "px")


transition : Html.Attribute Msg
transition =
    class "transition-all duration-500"
