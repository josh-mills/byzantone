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
import Model exposing (Layout(..), LayoutSelection(..), Modal(..), Model, layoutFor, layoutString)
import Movement exposing (Movement(..))
import RadioFieldset
import Styles
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
            [ class "lg:container lg:mx-auto font-serif"
            , case layoutFor model.layout of
                Portrait ->
                    class "flex flex-row flex-wrap-reverse"

                Landscape ->
                    Styles.flexCol
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
        , Styles.transition
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
    Html.header [ Styles.flexRowCentered ]
        [ div [ class "w-7" ] []
        , div [ Styles.flexCol, class "flex-1 mb-4 mx-4" ]
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
                    , Styles.transition
                    , onClick (SelectModal modal)
                    ]
                    [ text (Model.modalToString modal) ]
                ]
    in
    Html.ul
        [ class "fixed top-0 right-0 z-50 bg-white shadow-md"
        , Styles.borderRounded
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
                , Styles.flexCol
                , class "p-6 bg-white"
                , Styles.borderRounded
                , class "shadow-md font-serif"
                , id "modal"
                ]
                [ h2 [ Styles.flexRow, class "justify-between mb-1" ]
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
    div [ Styles.flexCol, class "gap-2" ]
        [ spacingButton model.showSpacing
            |> viewIf debuggingLayout
        , RadioFieldset.view
            { itemToString = layoutString
            , legendText = "Layout"
            , onSelect = SetLayout
            , options = [ Auto, Manual Portrait, Manual Landscape ]
            , selected = model.layout.layoutSelection
            , viewItem = Nothing
            }
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
        [ id "pitch-space"
        , class "flex-nowrap"
        , Styles.transition
        , case layoutFor model.layout of
            Portrait ->
                class "flex flex-row min-w-[360px]"

            Landscape ->
                Styles.flexCol
        ]
        [ intervalCol model intervals
        , pitchCol model intervals
        ]


intervalCol : Model -> List Interval -> Html Msg
intervalCol model intervals =
    let
        definedIntervals =
            List.map (viewInterval model) intervals

        spacerTop =
            List.last intervals
                |> viewMaybe (spacerInterval model)

        spacerBottom =
            List.head intervals
                |> viewMaybe (spacerInterval model)
                |> List.singleton
    in
    div
        [ case layoutFor model.layout of
            Portrait ->
                class "flex flex-col-reverse w-36"

            Landscape ->
                class "flex flex-row h-24"
        , onMouseLeave (SelectProposedMovement None)
        ]
        (spacerTop :: definedIntervals ++ spacerBottom)


viewInterval : Model -> Interval -> Html Msg
viewInterval ({ currentPitch, proposedMovement } as model) interval =
    let
        viewIntervalCharacter degree =
            currentPitch
                |> Maybe.map (\current -> Degree.getInterval current degree)
                |> Maybe.andThen basicInterval
                |> Html.Extra.viewMaybe
                    (Interval.view >> List.singleton >> span [ class "me-2" ])

        ( maybeIntervalCharacter, movementAttrs, element ) =
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

        ( elementLayoutAttrs, spanLayoutAttrs ) =
            case layoutFor model.layout of
                Portrait ->
                    ( [ Styles.flexRowCentered
                      , class "w-full"
                      , Styles.height (interval.moria * heightFactor model.layout.viewport)
                      ]
                    , [ Styles.flexRow, class "my-auto" ]
                    )

                Landscape ->
                    ( [ Styles.flexRowCentered
                      , class "h-full"
                      , Styles.width (interval.moria * widthFactor model.layout.viewport)
                      ]
                    , [ class "flex flex-col self-center" ]
                    )
    in
    element
        ([ Styles.border
         , Styles.transition
         , classList
            [ ( "bg-slate-200"
              , shouldHighlight currentPitch proposedMovement interval
              )
            , ( "hover:bg-slate-200", Maybe.isJust currentPitch )
            ]
         ]
            ++ elementLayoutAttrs
            ++ movementAttrs
        )
        [ span spanLayoutAttrs
            [ maybeIntervalCharacter
            , span [ class "text-gray-600" ] [ text (String.fromInt interval.moria) ]
            ]
        ]


spacerInterval : Model -> Interval -> Html Msg
spacerInterval ({ showSpacing } as model) { moria } =
    div
        [ case layoutFor model.layout of
            Portrait ->
                Styles.height (moria * heightFactor model.layout.viewport // 2)

            Landscape ->
                Styles.width (moria * widthFactor model.layout.viewport // 2)
        , Styles.transition
        , classList
            [ ( "text-center bg-slate-300", showSpacing )
            , ( "pt-9", True )
            ]
        ]
        [ viewIf showSpacing <| text <| "(" ++ String.fromInt (moria // 2) ++ ")" ]


pitchCol : Model -> List Interval -> Html Msg
pitchCol model intervals =
    intervalsToPitchHeights intervals
        |> List.map (viewPitch model)
        |> div
            [ case layoutFor model.layout of
                Portrait ->
                    class "flex flex-col-reverse w-16"

                Landscape ->
                    class "flex flex-row h-16"
            ]


viewPitch : Model -> PitchHeight -> Html Msg
viewPitch ({ scale, showSpacing, currentPitch, proposedMovement } as model) pitchHeight =
    let
        degree =
            Just pitchHeight.degree

        isCurrentPitch =
            degree == currentPitch

        layout =
            layoutFor model.layout

        spanAttrs =
            case layout of
                Portrait ->
                    [ style "padding-top" <| String.fromInt (pitchHeight.aboveCenter * (heightFactor model.layout.viewport - 2)) ++ "px"
                    , Styles.flexRow
                    , class "gap-2 absolute"
                    ]

                Landscape ->
                    [ Styles.flexCol
                    , class "gap-4"
                    , style "padding-left" <| String.fromInt (pitchHeight.belowCenter * (widthFactor model.layout.viewport - 6)) ++ "px"
                    ]

        --     -- for dev purposes only; will eventually be deleted
        -- totalHeight =
        --     pitchHeight.aboveCenter + pitchHeight.belowCenter |> String.fromInt
        -- spacingText =
        --     "(" ++ totalHeight ++ " = " ++ String.fromInt pitchHeight.belowCenter ++ " + " ++ String.fromInt pitchHeight.aboveCenter ++ ")"
    in
    div
        [ classList [ ( "border border-gray-500 bg-slate-200", showSpacing ) ]
        , Styles.transition
        , Styles.flexRowCentered
        , case layout of
            Portrait ->
                Styles.height <| (pitchHeight.belowCenter + pitchHeight.aboveCenter) * heightFactor model.layout.viewport

            Landscape ->
                Styles.width <| (pitchHeight.belowCenter + pitchHeight.aboveCenter) * widthFactor model.layout.viewport
        ]
        [ button
            [ class "flex px-4 w-full rounded-full"
            , id <| "p_" ++ Degree.toString pitchHeight.degree
            , Styles.transition
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
                (Styles.transition
                    :: classList [ ( "text-red-600", isCurrentPitch ) ]
                    :: spanAttrs
                )
                [ div
                    [ class "text-xl sm:text-3xl relative"
                    , case layout of
                        Portrait ->
                            class "-top-5"

                        Landscape ->
                            class "-top-2"
                    ]
                    [ viewMaybe (Martyria.view << Martyria.for scale) degree ]
                , viewIf isCurrentPitch <|
                    div [ class "w-4 ms-2" ] [ Icons.xmark ]

                -- , viewIf showSpacing <| span [ class "ms-2" ] [ text spacingText ]
                ]
            ]
        ]



-- CONTROLS


viewControls : Model -> Html Msg
viewControls model =
    div [ class "w-max", classList [ ( "mt-8", debuggingLayout ) ] ]
        [ selectScale model
        , viewCurrentPitch model.currentPitch
        , gainInput model.audioSettings
        ]


spacingButton : Bool -> Html Msg
spacingButton showSpacing =
    button
        [ Styles.buttonClass
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
        [ Styles.buttonClass
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
            [ Styles.buttonClass
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


heightFactor : Dom.Viewport -> Int
heightFactor viewport =
    (viewport.viewport.height / 100)
        |> truncate
        |> clamp 6 12


widthFactor : Dom.Viewport -> Int
widthFactor viewport =
    (viewport.viewport.width / 100)
        |> truncate
        |> clamp 6 18


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
