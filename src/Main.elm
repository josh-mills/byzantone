module Main exposing (main)

import Array
import Browser
import Byzantine.ByzHtml.Martyria as Martyria
import Byzantine.Degree as Degree exposing (Degree(..))
import Byzantine.Martyria as Martyria
import Byzantine.Scale as Scale exposing (Scale(..))
import Html exposing (Html, button, div, fieldset, input, label, legend, span, text)
import Html.Attributes exposing (checked, class, classList, for, id, style, type_)
import Html.Events exposing (onClick)
import Html.Extra exposing (viewIf, viewMaybe)
import List.Extra as List



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
      }
    , Cmd.none
    )



-- MODEL


type alias Model =
    { scale : Scale
    , showSpacing : Bool
    , currentPitch : Maybe Degree
    }



-- TYPES


type alias Interval =
    Int


type alias PitchHeight =
    { belowCenter : Int
    , aboveCenter : Int
    }


intervalsToPitches : List Interval -> List PitchHeight
intervalsToPitches intervals =
    let
        go : PitchHeight -> List Interval -> List PitchHeight
        go prev rest =
            case rest of
                [] ->
                    []

                x :: [] ->
                    [ { belowCenter = prev.aboveCenter, aboveCenter = x // 2 }
                    , { belowCenter = x // 2, aboveCenter = x // 2 }
                    ]

                x :: xs ->
                    let
                        next =
                            { belowCenter = prev.aboveCenter
                            , aboveCenter = x // 2
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
                    { belowCenter = x // 2
                    , aboveCenter = x // 2
                    }
            in
            firstPitch :: go firstPitch xs



-- UPDATE


type Msg
    = SelectPitch (Maybe Degree)
    | SetScale Scale
    | ToggleSpacing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectPitch pitch ->
            ( { model | currentPitch = pitch }
            , Cmd.none
            )

        SetScale scale ->
            ( { model | scale = scale, currentPitch = Nothing }
            , Cmd.none
            )

        ToggleSpacing ->
            ( { model | showSpacing = not model.showSpacing }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container m-4 flex flex-row" ]
        [ Degree.baseOctaveIntervals model.scale
            |> pitchSpace model
        , viewControls model
        ]


pitchSpace : Model -> List Interval -> Html Msg
pitchSpace model intervals =
    div
        [ class "flex flex-row flex-nowrap transition-all duration-500"
        , class "min-w-[360px]"
        ]
        [ intervalCol model.showSpacing intervals
        , pitchCol model intervals
        ]


intervalCol : Bool -> List Int -> Html Msg
intervalCol showSpacing intervals =
    let
        definedIntervals =
            intervals
                |> List.reverse
                |> List.map viewInterval

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


viewInterval : Int -> Html Msg
viewInterval i =
    div
        [ class "border border-gray-300"
        , class "flex flex-row justify-center"
        , height (i * 10)
        , transition
        ]
        [ span [ class "my-auto" ] [ text (String.fromInt i) ] ]


spacerInterval : Bool -> Int -> Html Msg
spacerInterval showSpacing i =
    div
        [ height (i * 10 // 2)
        , transition
        , classList [ ( "text-center bg-slate-300", showSpacing ) ]
        ]
        [ viewIf showSpacing <| text <| "(" ++ String.fromInt (i // 2) ++ ")" ]


pitchCol : Model -> List Int -> Html Msg
pitchCol model intervals =
    intervalsToPitches intervals
        |> List.indexedMap (viewPitch model)
        |> List.reverse
        |> div []


viewPitch : Model -> Int -> PitchHeight -> Html Msg
viewPitch { scale, showSpacing, currentPitch } ordinal pitchHeight =
    let
        degree =
            Degree.baseOctave scale
                |> Array.get ordinal

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
            , onClick (SelectPitch degree)
            ]
            [ span
                [ style "padding-top" <| String.fromInt (pitchHeight.aboveCenter * 8) ++ "px"
                , transition
                , class "flex flex-row gap-2 absolute"
                , classList [ ( "text-red-600", degree == currentPitch ) ]
                ]
                [ div [ class "text-3xl relative -top-5" ]
                    [ viewMaybe (Martyria.view << Martyria.for scale) degree ]
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
