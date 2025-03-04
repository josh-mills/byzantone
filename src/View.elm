module View exposing (view)

import AudioSettings exposing (AudioSettings)
import Browser.Dom as Dom
import Byzantine.ByzHtml.Interval as Interval
import Byzantine.ByzHtml.Martyria as Martyria
import Byzantine.Degree as Degree exposing (Degree(..))
import Byzantine.IntervalCharacter exposing (..)
import Byzantine.Martyria as Martyria
import Byzantine.Pitch as Pitch exposing (Interval, PitchStandard(..), Register(..))
import Byzantine.Scale as Scale exposing (Scale(..))
import Copy
import Html exposing (Html, button, div, h1, h2, input, main_, p, span, text)
import Html.Attributes as Attr exposing (class, classList, id, style, type_)
import Html.Attributes.Extra as Attr
import Html.Events exposing (onClick, onFocus, onInput, onMouseEnter, onMouseLeave)
import Html.Extra exposing (viewIf, viewMaybe)
import Icons
import Json.Decode exposing (Decoder)
import List.Extra as List
import Maybe.Extra as Maybe
import Model exposing (Modal(..), Model)
import Movement exposing (Movement(..))
import RadioFieldset
import Update exposing (Msg(..))


debuggingLayout : Bool
debuggingLayout =
    False



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "p-4" ]
        [ audio model
        , backdrop model
        , header model
        , viewModal model
        , viewIf model.showSpacing (div [ class "text-center" ] [ text "|" ])
        , viewIf model.menuOpen menu
        , main_
            [ class "lg:container lg:mx-auto flex flex-row flex-wrap-reverse font-serif"
            , Html.Events.on "keydown" keyDecoder
            , Attr.attributeIf model.menuOpen (onClick ToggleMenu)
            ]
            [ pitchSpace model
            , viewControls model
            ]
        ]


backdrop : Model -> Html Msg
backdrop model =
    let
        show =
            model.menuOpen || Model.modalOpen model.modal
    in
    div
        [ class "fixed top-0 left-0 w-full h-full"
        , transition
        , classList
            [ ( "-z-10", not show )
            , ( "bg-slate-400 opacity-40 z-10", show )
            ]
        , Attr.attributeIf model.menuOpen (onClick ToggleMenu)
        , Attr.attributeIf (Model.modalOpen model.modal) (onClick (SelectModal NoModal))
        ]
        []


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
                Pitch.frequency model.audioSettings.pitchStandard model.audioSettings.register model.scale pitch
                    |> String.fromFloat
                    |> Attr.attribute "ison"
        ]
        []


header : Model -> Html Msg
header model =
    Html.header
        [ class "flex flex-row justify-center"
        ]
        [ div [ class "w-7" ] []
        , div [ class "flex-1 flex flex-col mb-4 mx-4" ]
            [ h1 [ class "font-heading text-4xl text-center" ]
                [ text "ByzanTone" ]
            , p [ class "font-serif text-center" ]
                [ text "A tool for learning the pitches and intervals of Byzantine chant." ]
            , viewIf model.showSpacing (p [ class "text-center" ] [ text "|" ])
            ]
        , button
            [ class "w-7 mt-2 self-start"
            , onClick ToggleMenu
            ]
            [ Icons.bars ]
        ]


{-| TODO: give this a drawer effect
-}
menu : Html Msg
menu =
    let
        menuItem modal =
            Html.li []
                [ button
                    [ class "p-2 hover:bg-gray-200 w-full"
                    , transition
                    , onClick (SelectModal modal)
                    ]
                    [ text (Model.modalToString modal) ]
                ]
    in
    Html.ul
        [ class "fixed top-0 right-0 z-50 bg-white border border-gray-300 rounded-md shadow-md"
        , class "font-serif"
        , id "menu"
        , Html.Events.on "keydown" keyDecoder
        ]
        [ menuItem AboutModal
        , menuItem SettingsModal
        ]


{-| TODO: positioning still needs work.
-}
viewModal : Model -> Html Msg
viewModal model =
    case model.modal of
        NoModal ->
            Html.Extra.nothing

        _ ->
            Html.node "dialog"
                [ class "fixed inset-1/4 top-24 w-3/4 md:w-1/2 z-10"
                , class "flex flex-col"
                , class "p-6 bg-white"
                , class "border border-gray-300 rounded-md shadow-md font-serif"
                , id "modal"
                ]
                [ h2 [ class "flex flex-row justify-between mb-1" ]
                    [ span [ class "font-heading text-2xl" ]
                        [ text (Model.modalToString model.modal) ]
                    , button
                        [ class "w-8 p-2"
                        , onClick (SelectModal NoModal)
                        ]
                        [ Icons.xmark ]
                    ]
                , modalContent model
                ]


modalContent : Model -> Html Msg
modalContent model =
    case model.modal of
        NoModal ->
            Html.Extra.nothing

        AboutModal ->
            Copy.about

        SettingsModal ->
            settings model


settings : Model -> Html Msg
settings model =
    div [ class "flex flex-col gap-2" ]
        [ spacingButton model.showSpacing
            |> viewIf debuggingLayout
        , RadioFieldset.view
            { itemToString =
                \register ->
                    case register of
                        Treble ->
                            "Treble"

                        Bass ->
                            "Bass"
            , legendText = "Register"
            , onSelect = SetRegister
            , options = [ Treble, Bass ]
            , selected = model.audioSettings.register
            , viewItem = Nothing
            }
        , RadioFieldset.view
            { itemToString =
                \pitchStandard ->
                    case pitchStandard of
                        Ni256 ->
                            "Ni256"

                        Ke440 ->
                            "Ke440"
            , legendText = "Pitch Standard"
            , onSelect = SetPitchStandard
            , options = [ Ni256, Ke440 ]
            , selected = model.audioSettings.pitchStandard
            , viewItem = Just viewPitchStandard
            }
        , gainInput model.audioSettings
        ]


viewPitchStandard : PitchStandard -> Html msg
viewPitchStandard pitchStandard =
    let
        ( martyria, frequency ) =
            case pitchStandard of
                Ni256 ->
                    ( Martyria.for Diatonic Ni |> Martyria.view
                    , " = 256 Hz"
                    )

                Ke440 ->
                    ( Martyria.for Diatonic Ke |> Martyria.view
                    , " = 440 Hz"
                    )
    in
    div [ class "mb-1" ]
        [ span [ class "text-xl relative bottom-1.5" ] [ martyria ]
        , text frequency
        ]



-- PITCH SPACE


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
        [ selectScale model
        , viewCurrentPitch model.currentPitch
        , gainInput model.audioSettings
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
    RadioFieldset.view
        { itemToString = Scale.name
        , legendText = "Select Scale"
        , onSelect = SetScale
        , options = Scale.all
        , selected = model.scale
        , viewItem = Nothing
        }


viewCurrentPitch : Maybe Degree -> Html Msg
viewCurrentPitch pitch =
    div [ class "mt-2" ]
        [ text <| "Current Pitch: "
        , case pitch of
            Nothing ->
                text "none"

            Just p ->
                Degree.text p
        , viewIf (Maybe.isJust pitch) clearPitchButton
        ]


clearPitchButton : Html Msg
clearPitchButton =
    button
        [ buttonClass
        , class "mx-2"
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

TODO: move to own module?

-}
type alias PitchHeight =
    { degree : Degree
    , belowCenter : Int
    , aboveCenter : Int
    }


intervalsToPitchHeights : List Interval -> List PitchHeight
intervalsToPitchHeights intervals =
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
            firstPitch :: intervalsToPitchHeightsHelper firstPitch xs


intervalsToPitchHeightsHelper : PitchHeight -> List Interval -> List PitchHeight
intervalsToPitchHeightsHelper prev rest =
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
            next :: intervalsToPitchHeightsHelper next xs
