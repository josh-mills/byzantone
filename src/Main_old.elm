module Main_old exposing (main)

import Browser
import ByzHtmlNeumes as Byz
import Html exposing (Html, button, div, h2, input, p, span, text)
import Html.Attributes as Attr exposing (attribute, class, style, type_)
import Html.Events exposing (onClick, onInput)



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
      , playingFrequency = Nothing
      , gain = 0.5
      }
    , Cmd.none
    )



-- MODEL


type alias Model =
    { scale : Scale
    , playingFrequency : Maybe Float
    , gain : Float
    }


type Scale
    = Diatonic
    | Enharmonic


scaleToString : Scale -> String
scaleToString scale =
    case scale of
        Diatonic ->
            "Diatonic"

        Enharmonic ->
            "Enharmonic"


{-| Still thinking about this... What's the point? Also, we may need some sort
of octave designations (perhaps `Νη_`, `Ζω_`, etc.) and chromaticism.
-}
type Παραλλαγή
    = Πα
    | Βου
    | Γα
    | Δι
    | Κε
    | Ζω
    | Νη



-- UPDATE


type Msg
    = Start
    | Stop
    | StartWithFrequency Float
    | ToggleScale
    | SetGain String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetGain gain ->
            ( { model
                | gain = clamp 0 1 <| Maybe.withDefault model.gain <| String.toFloat gain
              }
            , Cmd.none
            )

        Start ->
            ( { model | playingFrequency = Just referencePitch }, Cmd.none )

        Stop ->
            ( { model | playingFrequency = Nothing }, Cmd.none )

        StartWithFrequency freq ->
            ( { model | playingFrequency = Just freq }, Cmd.none )

        ToggleScale ->
            ( { model
                | scale =
                    case model.scale of
                        Diatonic ->
                            Enharmonic

                        Enharmonic ->
                            Diatonic
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "flex flex-col font-serif" ]
        [ chantEngine model
        , testButton model
        , div [ class "flex flex-row" ]
            [ div [ class "flex flex-row" ] [ viewScale model.scale ]
            , controls model
            ]
        , div [ class "flex flex-col m-6 gap-2" ]
            [ p [] [ text "trying out the byz html thing" ]
            , p []
                [ span [ class "text-4xl" ]
                    [ Byz.martyria_pa_alpha
                    , Byz.martyria_bou_legetos
                    , Byz.martyria_ga_nana
                    , Byz.martyria_di_delta
                    , Byz.martyria_ke_alpha
                    , Byz.martyria_zo_high_legetos
                    , Byz.martyria_ni_high_nana
                    , Byz.martyria_pa_high_alpha
                    ]
                ]

            -- [ testSign
            -- , note [ ison, psifiston, lyric "Lord" ]
            -- ]
            ]
        ]


testButton : Model -> Html Msg
testButton model =
    div [ class "w-full" ]
        [ case model.playingFrequency of
            Just _ ->
                button
                    [ onClick Stop
                    , class "bg-gray-400 w-full"
                    ]
                    [ text "stop" ]

            Nothing ->
                button
                    [ onClick Start
                    , class "bg-gray-400 w-full"
                    ]
                    [ text "play" ]
        ]



-- CONTROLS


controls : Model -> Html Msg
controls model =
    let
        playButton steps =
            button
                [ onClick (StartWithFrequency <| frequencyFromΠα steps)
                , class "bg-gray-100 m-2 p-2 rounded-md"
                ]
                [ text <| "play " ++ String.fromInt steps ]
    in
    div []
        [ h2 [] [ text "controls" ]
        , div []
            [ button
                [ onClick ToggleScale ]
                [ text <| scaleToString model.scale ]
            ]
        , gainControl model
        , div [] <|
            List.map playButton (runningSum <| intervalsFor model.scale)
        ]


gainControl : Model -> Html Msg
gainControl model =
    input
        [ type_ "range"
        , Attr.min "0"
        , Attr.max "1"
        , Attr.step "0.02"
        , Attr.value <| String.fromFloat model.gain
        , onInput SetGain
        ]
        []



-- SCALE REPRESENTATION


viewScale : Scale -> Html Msg
viewScale scale =
    div [ class "flex flex-col m-4" ]
        (List.map viewInterval <| List.reverse <| intervalsFor scale)


{-| Heights of the intervals are calculated based on the interval size
multiplied by a fixed factor. This factor should be adjusted so that the full
extent of the scale is not greater than the height of the viewport (or width, if
we're dealing with a landscape-orientation for a mobile device).
-}
viewInterval : Int -> Html Msg
viewInterval steps =
    div
        [ class "flex flex-col w-36 border-2 border-gray-400 text-center transition-[height] justify-center"
        , style "height" <| (String.fromInt <| steps * 8) ++ "px"
        ]
        [ span [] [ text <| String.fromInt steps ] ]



-- PITCH PROCESSING


{-| This is an equal-tempered D4 (=Πα) based on A440. An alternative tuning
standard puts this slightly lower, based on C4 = 256 Hz. We might want want to
enable a pitch standard toggle in an advanced settings pane somewhere: Κε = 440
| Νη = 256.
-}
referencePitch : Float
referencePitch =
    293.67


{-| Calculate the frequency for a tone some number of equal-tempered
`stepsRemoved` from the `referenceFrequency`. The equal-tempered frame of
reference here is 72 steps (_μόρια_) per octave.
-}
frequency : Float -> Int -> Float
frequency referenceFrequency stepsRemoved =
    2 ^ (toFloat stepsRemoved / 72) * referenceFrequency


frequencyFromΠα =
    frequency referencePitch


{-| All these should sum to 72.
-}
intervalsFor : Scale -> List Int
intervalsFor scale =
    case scale of
        Diatonic ->
            [ 10, 8, 12, 12, 10, 8, 12 ]

        Enharmonic ->
            [ 12, 6, 12, 12, 6, 12, 12 ]


runningSum : List Int -> List Int
runningSum ints =
    List.foldl
        (\next acc -> (next + Maybe.withDefault 0 (List.head acc)) :: acc)
        [ 0 ]
        ints
        |> List.reverse



-- AUDIO COMPONENT


chantEngine : Model -> Html Msg
chantEngine model =
    Html.node "chant-engine"
        [ attribute "ison" <| Maybe.withDefault "" <| Maybe.map String.fromFloat model.playingFrequency
        , attribute "gain" <| String.fromFloat model.gain
        , attribute "melos" ""
        ]
        []
