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
            ( { model | scale = scale }
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
            |> pitchSpace model.scale model.showSpacing
        , viewControls model
        ]


pitchSpace : Scale -> Bool -> List Interval -> Html Msg
pitchSpace scale showSpacing intervals =
    div
        [ class "flex flex-row flex-nowrap transition-all duration-500"
        , class "min-w-[360px]"
        ]
        [ intervalCol showSpacing intervals
        , pitchCol scale showSpacing intervals
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


pitchCol : Scale -> Bool -> List Int -> Html Msg
pitchCol scale showSpacing intervals =
    intervalsToPitches intervals
        |> List.indexedMap (viewPitch scale showSpacing)
        |> List.reverse
        |> div []


viewPitch : Scale -> Bool -> Int -> PitchHeight -> Html Msg
viewPitch scale showSpacing ordinal pitchHeight =
    let
        degree =
            Degree.baseOctave scale
                |> Array.get ordinal

        totalHeight =
            pitchHeight.aboveCenter + pitchHeight.belowCenter |> String.fromInt
    in
    div
        [ classList [ ( "border border-gray-500 bg-slate-200", showSpacing ) ]
        , class "flex flex-row justify-center"
        , height <| (pitchHeight.belowCenter + pitchHeight.aboveCenter) * 10
        , transition
        ]
        [ button
            [ class "flex px-4 w-full min-w-[40px] hover:bg-slate-200"
            , classList [ ( "bg-green-300", showSpacing ) ]
            , onClick (SelectPitch degree)
            ]
            [ span
                [ style "padding-top" <| String.fromInt (pitchHeight.aboveCenter * 8) ++ "px"
                , transition
                , class "flex flex-row gap-2 absolute"
                ]
                [ div
                    [ class "text-3xl" -- can I adjust position on this ?
                    ]
                    [ viewMaybe (Martyria.view << Martyria.for scale) degree ]

                --     [ Martyria.view (Martyria.for scale Degree.Pa)
                --     ]
                , viewIf showSpacing <| text <| " (" ++ totalHeight ++ " = " ++ String.fromInt pitchHeight.belowCenter ++ " + " ++ String.fromInt pitchHeight.aboveCenter ++ ")"
                ]
            ]
        ]



-- CONTROLS


viewControls : Model -> Html Msg
viewControls model =
    div []
        [ spacingButton model.showSpacing
        , selectScale model

        -- , viewCurrentPitch model.currentPitch
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


viewCurrentPitch : Maybe Int -> Html Msg
viewCurrentPitch pitch =
    div [ class "mt-2" ]
        [ text <| "Current Pitch: "
        , case pitch of
            Nothing ->
                text "none"

            Just p ->
                text <| String.fromInt p
        ]



-- HELPERS


{-| in px
-}
height : Int -> Html.Attribute Msg
height h =
    style "height" (String.fromInt h ++ "px")


transition : Html.Attribute Msg
transition =
    class "transition-all duration-500"
