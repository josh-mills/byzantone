module View exposing (view)

import Byzantine.Accidental as Accidental exposing (Accidental)
import Byzantine.ByzHtml.Accidental as Accidental
import Byzantine.ByzHtml.Martyria as Martyria
import Byzantine.Degree as Degree exposing (Degree(..))
import Byzantine.IntervalCharacter exposing (..)
import Byzantine.Martyria as Martyria
import Byzantine.Pitch as Pitch exposing (Pitch, PitchStandard(..), Register(..))
import Byzantine.Scale as Scale exposing (Scale(..))
import Copy
import Html exposing (Html, button, datalist, div, h1, h2, input, main_, p, span, text)
import Html.Attributes as Attr exposing (class, classList, id, type_)
import Html.Attributes.Extra as Attr
import Html.Events exposing (onClick, onInput)
import Html.Extra exposing (viewIf, viewIfLazy)
import Html.Lazy exposing (lazy, lazy2, lazy3, lazy4)
import Icons
import Json.Decode exposing (Decoder)
import List.Extra as List
import Maybe.Extra as Maybe
import Model exposing (Modal(..), Model)
import Model.AudioSettings exposing (AudioSettings)
import Model.LayoutData as LayoutData exposing (Layout(..), LayoutData, LayoutSelection(..), layoutFor)
import Model.ModeSettings exposing (ModeSettings)
import Model.PitchState as PitchState exposing (IsonStatus, PitchState)
import Movement exposing (Movement(..))
import RadioFieldset
import Styles
import Update exposing (Msg(..))
import View.PitchSpace as PitchSpace


debuggingLayout : Bool
debuggingLayout =
    False



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "p-4" ]
        [ lazy4 chantEngineNode
            model.audioSettings
            model.modeSettings.scale
            model.pitchState.currentPitch
            (PitchState.ison model.pitchState.ison)
        , lazy2 backdrop model.menuOpen model.modal
        , header
        , lazy4 viewModal model.audioSettings model.layoutData model.modeSettings model.modal

        -- , viewIf model.layoutData.showSpacing (div [ class "text-center" ] [ text "|" ])
        , viewIf model.menuOpen menu
        , main_
            [ class "lg:container lg:mx-auto font-serif"
            , case layoutFor model.layoutData of
                Vertical ->
                    class "flex flex-row flex-wrap-reverse"

                Horizontal ->
                    Styles.flexCol
            , Html.Events.on "keydown" keyDecoder
            , Attr.attributeIf model.menuOpen (onClick ToggleMenu)
            ]
            [ lazy3 PitchSpace.view model.layoutData model.modeSettings model.pitchState
            , lazy3 viewControls model.audioSettings model.modeSettings model.pitchState
            ]
        ]


backdrop : Bool -> Modal -> Html Msg
backdrop menuOpen modal =
    let
        show =
            menuOpen || Model.modalOpen modal
    in
    div
        [ class "fixed top-0 left-0 w-full h-full"
        , Styles.transition
        , classList
            [ ( "-z-10", not show )
            , ( "bg-slate-400 opacity-40 z-10", show )
            ]
        , Attr.attributeIf menuOpen (onClick ToggleMenu)
        , Attr.attributeIf (Model.modalOpen modal) (onClick (SelectModal NoModal))
        ]
        []


chantEngineNode : AudioSettings -> Scale -> Maybe Pitch -> Maybe Pitch -> Html msg
chantEngineNode audioSettings scale currentPitch currentIson =
    let
        frequency pitch =
            Pitch.frequency audioSettings.pitchStandard
                audioSettings.register
                scale
                pitch
                |> String.fromFloat
    in
    Html.node "chant-engine"
        [ audioSettings.gain
            |> String.fromFloat
            |> Attr.attribute "gain"
        , Maybe.unwrap Attr.empty
            (Attr.attribute "melos" << frequency)
            currentPitch
        , Maybe.unwrap Attr.empty
            (Attr.attribute "ison" << frequency)
            currentIson
        ]
        []


header : Html Msg
header =
    Html.header [ Styles.flexRowCentered ]
        [ div [ class "w-7" ] []
        , div [ Styles.flexCol, class "flex-1 mb-4 mx-4" ]
            [ h1 [ class "font-heading text-4xl text-center" ]
                [ text "ByzanTone" ]
            , p [ class "font-serif text-center" ]
                [ text "A tool for learning the pitches and intervals of Byzantine chant." ]

            -- , viewIf model.layoutData.showSpacing (p [ class "text-center" ] [ text "|" ])
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
viewModal : AudioSettings -> LayoutData -> ModeSettings -> Modal -> Html Msg
viewModal audioSettings layoutData modeSettings modal =
    case modal of
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
                        [ text (Model.modalToString modal) ]
                    , button
                        [ class "w-8 p-2"
                        , onClick (SelectModal NoModal)
                        ]
                        [ Icons.xmark ]
                    ]
                , modalContent audioSettings layoutData modeSettings modal
                ]


modalContent : AudioSettings -> LayoutData -> ModeSettings -> Modal -> Html Msg
modalContent audioSettings layoutData modeSettings modal =
    case modal of
        NoModal ->
            Html.Extra.nothing

        AboutModal ->
            Copy.about

        SettingsModal ->
            lazy3 settings audioSettings layoutData modeSettings


settings : AudioSettings -> LayoutData -> ModeSettings -> Html Msg
settings audioSettings layoutData modeSettings =
    div [ Styles.flexCol, class "gap-2" ]
        [ viewIfLazy debuggingLayout
            (\_ -> spacingButton layoutData.showSpacing)
        , lazy2 RadioFieldset.view layoutRadioConfig layoutData.layoutSelection
        , lazy2 RadioFieldset.view registerRadioConfig audioSettings.register
        , lazy2 RadioFieldset.view pitchStandardRadioConfig audioSettings.pitchStandard
        , lazy gainInput audioSettings
        , lazy rangeFieldset modeSettings
        ]


layoutRadioConfig : RadioFieldset.Config LayoutSelection Msg
layoutRadioConfig =
    { itemToString = LayoutData.layoutString
    , legendText = "Layout"
    , onSelect = SetLayout
    , options = [ Auto, Manual Vertical, Manual Horizontal ]
    , viewItem = Nothing
    }


registerRadioConfig : RadioFieldset.Config Register Msg
registerRadioConfig =
    { itemToString = Pitch.registerToString
    , legendText = "Register"
    , onSelect = SetRegister
    , options = [ Treble, Bass ]
    , viewItem = Nothing
    }


pitchStandardRadioConfig : RadioFieldset.Config PitchStandard Msg
pitchStandardRadioConfig =
    { itemToString = Pitch.pitchStandardToString
    , legendText = "Pitch Standard"
    , onSelect = SetPitchStandard
    , options = [ Ni256, Ke440 ]
    , viewItem = Just viewPitchStandard
    }


{-| Set the visible range of the pitch space. A minimum of a tetrachord is
required. Data validation is handled through the input options rather than
through the update logic; we may need to consider additional validation
depending on how dynamic we want to be, and on how well we should reflect
specific modal constraints.
-}
rangeFieldset : ModeSettings -> Html Msg
rangeFieldset { rangeStart, rangeEnd } =
    let
        maxLowerBound =
            Degree.step rangeEnd -3 |> Maybe.withDefault rangeStart

        minUpperBound =
            Degree.step rangeStart 3 |> Maybe.withDefault rangeEnd

        intString degree =
            Degree.indexOf degree |> String.fromInt

        row label id_ min max value msg =
            div [ Styles.flexRow, class "align-center" ]
                [ Html.label
                    [ Attr.for id_
                    , class "w-14 mt-1"
                    ]
                    [ text label ]
                , div [ Styles.flexCol ]
                    [ Html.input
                        [ type_ "range"
                        , id id_
                        , class "w-80"
                        , onInput msg
                        , Attr.list (id_ ++ "-datalist")
                        , Attr.min (intString min)
                        , Attr.max (intString max)
                        , Attr.value (intString value)
                        ]
                        []
                    , datalist
                        [ id (id_ ++ "-datalist")
                        , Styles.flexRow
                        , class "justify-between w-80"
                        ]
                        (Degree.range min max |> List.map (makeOption msg))
                    ]
                ]

        makeOption msg degree =
            Html.option
                [ Attr.value (intString degree)
                , Attr.attribute "label" (Degree.toStringGreek degree)
                , onClick (msg (intString degree))
                , class "cursor-pointer font-greek"
                ]
                []
    in
    Html.fieldset [ Styles.borderRounded, class "px-2 pb-1 mb-2" ]
        [ Html.legend [ class "px-1" ] [ Html.text "Range" ]
        , row "From:" "range-start" GA maxLowerBound rangeStart SetRangeStart
        , row "To:" "range-end" minUpperBound Ga_ rangeEnd SetRangeEnd
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



-- CONTROLS


viewControls : AudioSettings -> ModeSettings -> PitchState -> Html Msg
viewControls audioSettings modeSettings pitchState =
    div [ class "w-max", classList [ ( "mt-8", debuggingLayout ) ] ]
        [ lazy2 RadioFieldset.view scaleRadioConfig modeSettings.scale
        , lazy isonButton pitchState.ison
        , lazy viewIson (PitchState.ison pitchState.ison)
        , lazy viewCurrentPitch pitchState.currentPitch
        , lazy viewAccidentalButtons pitchState.proposedAccidental
        , lazy gainInput audioSettings
        ]


scaleRadioConfig : RadioFieldset.Config Scale Msg
scaleRadioConfig =
    { itemToString = Scale.name
    , legendText = "Select Scale"
    , onSelect = SetScale
    , options = Scale.all
    , viewItem = Nothing
    }


isonButton : IsonStatus -> Html Msg
isonButton ison =
    button
        [ Styles.buttonClass
        , id "select-ison-button"
        , onClick
            (SetIson
                (case ison of
                    PitchState.NoIson ->
                        PitchState.SelectingIson Nothing

                    PitchState.SelectingIson (Just ison_) ->
                        PitchState.Selected ison_

                    PitchState.SelectingIson Nothing ->
                        PitchState.NoIson

                    PitchState.Selected _ ->
                        PitchState.SelectingIson
                            (PitchState.ison ison
                                |> Maybe.map Pitch.unwrapDegree
                            )
                )
            )
        ]
        [ text "Select Ison" ]


viewIson : Maybe Pitch -> Html Msg
viewIson pitch =
    div [ class "mt-2" ]
        [ text <| "Current Ison: "
        , case pitch of
            Nothing ->
                text "none"

            Just p ->
                Degree.text (Pitch.unwrapDegree p)
        , viewIf (Maybe.isJust pitch) (clearButton (SetIson PitchState.NoIson))
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


viewCurrentPitch : Maybe Pitch -> Html Msg
viewCurrentPitch pitch =
    div [ class "mt-2" ]
        [ text <| "Current Pitch: "
        , case pitch of
            Nothing ->
                text "none"

            Just pitch_ ->
                span []
                    [ Degree.text (Pitch.unwrapDegree pitch_)
                    , Html.Extra.viewMaybe
                        (\accidental ->
                            span []
                                [ text " ("
                                , text (Accidental.toString accidental)
                                , text ")"
                                ]
                        )
                        (Pitch.unwrapAccidental pitch_)
                    ]
        , viewIf (Maybe.isJust pitch) (clearButton (SelectPitch Nothing Nothing))
        ]


viewAccidentalButtons : Maybe Accidental -> Html Msg
viewAccidentalButtons maybeAccidental =
    Html.fieldset
        [ Styles.borderRounded
        , class "px-2 pb-1 mb-2"
        , class "flex flex-row flex-wrap gap-2 mt-2"
        ]
        (Html.legend [ class "px-1" ] [ Html.text "Accidental" ]
            :: List.map (viewAccidentalButton maybeAccidental) Accidental.all
        )


viewAccidentalButton : Maybe Accidental -> Accidental -> Html Msg
viewAccidentalButton proposedAccidental accidental =
    let
        isCurrent =
            proposedAccidental == Just accidental
    in
    button
        [ Styles.buttonClass
        , class "text-3xl min-w-12"
        , classList
            [ ( "text-blue-700 border-2 border-blue-700", isCurrent )
            , ( "border-2 border-transparent", not isCurrent )
            ]
        , onClick
            (if isCurrent then
                SelectProposedAccidental Nothing

             else
                SelectProposedAccidental (Just accidental)
            )
        ]
        [ Accidental.view Accidental.InheritColor accidental ]


clearButton : Msg -> Html Msg
clearButton msg =
    button
        [ Styles.buttonClass
        , class "mx-2"
        , onClick msg
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


keyDecoder : Decoder Msg
keyDecoder =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.map Keydown
