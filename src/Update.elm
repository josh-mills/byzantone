module Update exposing (Msg(..), update)

import Array
import Browser.Dom as Dom
import Byzantine.Accidental exposing (Accidental)
import Byzantine.Degree as Degree exposing (Degree(..))
import Byzantine.Pitch as Pitch exposing (PitchStandard, Register)
import Byzantine.Scale exposing (Scale)
import Maybe.Extra as Maybe
import Model exposing (Modal, Model)
import Model.AudioSettings exposing (AudioSettings)
import Model.LayoutData exposing (LayoutData, LayoutSelection)
import Model.ModeSettings exposing (ModeSettings)
import Model.PitchState as PitchState exposing (IsonStatus(..), PitchState)
import Movement exposing (Movement)
import Platform.Cmd as Cmd
import Task


type Msg
    = DomResult (Result Dom.Error ())
    | GotPitchSpaceElement (Result Dom.Error Dom.Element)
    | GotViewport Dom.Viewport
    | ViewportResize Int Int
    | Keydown String
    | NoOp
    | SelectModal Modal
    | SelectPitch (Maybe Degree) (Maybe Movement)
    | SelectProposedAccidental (Maybe Accidental)
    | SelectProposedMovement Movement
    | SetGain Float
    | SetIson IsonStatus
    | SetLayout LayoutSelection
    | SetPitchStandard PitchStandard
    | SetRangeStart String
    | SetRangeEnd String
    | SetRegister Register
    | SetScale Scale
    | ToggleMenu
    | ToggleSpacing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Task.perform GotViewport Dom.getViewport )

        DomResult _ ->
            -- just for dev purposes
            ( model, Cmd.none )

        GotPitchSpaceElement elementResult ->
            ( case elementResult of
                Ok element ->
                    updateLayoutData
                        (\layoutData -> { layoutData | pitchSpace = element })
                        model

                Err _ ->
                    model
            , Cmd.none
            )

        GotViewport viewport ->
            ( updateLayoutData
                (\layoutData -> { layoutData | viewport = viewport })
                model
            , Task.attempt GotPitchSpaceElement (Dom.getElement "pitch-space")
            )

        ViewportResize _ _ ->
            ( model
            , Cmd.batch
                [ Task.perform GotViewport Dom.getViewport
                , Task.perform GotViewport Dom.getViewport
                ]
            )

        SelectModal modal ->
            ( { model | modal = modal, menuOpen = False }
            , if Model.modalOpen modal then
                focus "modal"

              else
                Cmd.none
            )

        SelectPitch degree maybeMovement ->
            ( updatePitchState
                (\pitchState ->
                    { pitchState
                        | currentPitch = Maybe.map (Pitch.from pitchState.proposedAccidental) degree
                        , proposedAccidental = Nothing
                        , proposedMovement =
                            if Maybe.isJust degree then
                                Maybe.withDefault model.pitchState.proposedMovement maybeMovement

                            else
                                Movement.None
                    }
                )
                model
            , Cmd.none
            )

        SelectProposedAccidental maybeAccidental ->
            ( updatePitchState
                (\pitchState -> { pitchState | proposedAccidental = maybeAccidental })
                model
            , Cmd.none
            )

        SelectProposedMovement movement ->
            ( updatePitchState
                (\pitchState -> { pitchState | proposedMovement = movement })
                model
            , Cmd.none
            )

        SetGain gain ->
            ( updateAudioSettings
                (\audioSettings -> { audioSettings | gain = clamp 0 1 gain })
                model
            , Cmd.none
            )

        SetIson ison ->
            ( updatePitchState
                (\pitchState -> { pitchState | ison = ison })
                model
            , Cmd.none
            )

        SetLayout layoutSelection ->
            ( updateLayoutData
                (\layoutData -> { layoutData | layoutSelection = layoutSelection })
                model
            , Task.perform GotViewport Dom.getViewport
            )

        SetPitchStandard pitchStandard ->
            ( updateAudioSettings
                (\audioSettings -> { audioSettings | pitchStandard = pitchStandard })
                model
            , Cmd.none
            )

        SetRangeStart start ->
            ( updateModeSettings
                (\modeSettings ->
                    { modeSettings
                        | rangeStart =
                            String.toInt start
                                |> Maybe.andThen (\i -> Array.get i Degree.gamut)
                                |> Maybe.withDefault modeSettings.rangeStart
                    }
                )
                model
            , Cmd.none
            )

        SetRangeEnd end ->
            ( updateModeSettings
                (\modeSettings ->
                    { modeSettings
                        | rangeEnd =
                            String.toInt end
                                |> Maybe.andThen (\i -> Array.get i Degree.gamut)
                                |> Maybe.withDefault modeSettings.rangeEnd
                    }
                )
                model
            , Cmd.none
            )

        SetRegister register ->
            ( updateAudioSettings
                (\audioSettings -> { audioSettings | register = register })
                model
            , Cmd.none
            )

        SetScale scale ->
            ( updateModeSettings
                (\modeSettings -> { modeSettings | scale = scale })
                -- , currentPitch = Nothing -- consider this.
                model
            , Cmd.none
            )

        ToggleMenu ->
            ( { model | menuOpen = not model.menuOpen }
            , if not model.menuOpen then
                focus "menu"

              else
                Cmd.none
            )

        ToggleSpacing ->
            ( updateLayoutData
                (\layoutData -> { layoutData | showSpacing = not layoutData.showSpacing })
                model
            , Cmd.none
            )

        Keydown key ->
            let
                moveAndFocus i =
                    let
                        -- TODO: this needs to account for possible accidentals
                        pitch =
                            Maybe.map (Pitch.mapDegree (\d -> Degree.step d i |> Maybe.withDefault d)) model.pitchState.currentPitch
                    in
                    ( updatePitchState
                        (\pitchState -> { pitchState | currentPitch = pitch })
                        model
                    , Maybe.unwrap Cmd.none
                        (Pitch.unwrapDegree
                            >> Degree.toString
                            >> (++) "p_"
                            >> Dom.focus
                            >> Task.attempt DomResult
                        )
                        pitch
                    )

                setAndFocus degree =
                    ( case model.pitchState.ison of
                        PitchState.SelectingIson _ ->
                            updatePitchState
                                (\pitchState -> { pitchState | ison = PitchState.Selected degree })
                                model

                        _ ->
                            updatePitchState
                                (\pitchState ->
                                    { pitchState
                                        | currentPitch = Just (Pitch.from pitchState.proposedAccidental degree)
                                        , proposedAccidental = Nothing
                                    }
                                )
                                model
                    , Degree.toString degree |> (++) "p_" |> Dom.focus |> Task.attempt DomResult
                    )
            in
            case key of
                "ArrowUp" ->
                    moveAndFocus 1

                "ArrowDown" ->
                    moveAndFocus -1

                "Escape" ->
                    if model.menuOpen then
                        ( { model | menuOpen = False }
                        , Task.attempt DomResult (Dom.blur "menu")
                        )

                    else
                        ( setPitchState PitchState.initialPitchState model
                        , Cmd.none
                        )

                "1" ->
                    moveAndFocus 1

                "2" ->
                    moveAndFocus 2

                "3" ->
                    moveAndFocus 3

                "4" ->
                    moveAndFocus 4

                "5" ->
                    moveAndFocus 5

                "6" ->
                    moveAndFocus 6

                "7" ->
                    moveAndFocus 7

                "8" ->
                    moveAndFocus 8

                "9" ->
                    moveAndFocus 9

                "!" ->
                    moveAndFocus -1

                "@" ->
                    moveAndFocus -2

                "#" ->
                    moveAndFocus -3

                "$" ->
                    moveAndFocus -4

                "%" ->
                    moveAndFocus -5

                "^" ->
                    moveAndFocus -6

                "&" ->
                    moveAndFocus -7

                "*" ->
                    moveAndFocus -8

                "(" ->
                    moveAndFocus -9

                "n" ->
                    setAndFocus Ni

                "p" ->
                    setAndFocus Pa

                "b" ->
                    setAndFocus Bou

                "v" ->
                    setAndFocus Bou

                "g" ->
                    setAndFocus Ga

                "d" ->
                    setAndFocus Di

                "k" ->
                    setAndFocus Ke

                "z" ->
                    setAndFocus Zo_

                "i" ->
                    case model.pitchState.ison of
                        PitchState.NoIson ->
                            ( updatePitchState
                                (\pitchState ->
                                    { pitchState | ison = PitchState.SelectingIson Nothing }
                                )
                                model
                            , Task.attempt DomResult (Dom.focus "select-ison-button")
                            )

                        PitchState.SelectingIson (Just ison) ->
                            ( updatePitchState
                                (\pitchState ->
                                    { pitchState
                                        | ison = PitchState.Selected ison
                                    }
                                )
                                model
                            , Cmd.none
                            )

                        PitchState.SelectingIson Nothing ->
                            ( updatePitchState
                                (\pitchState ->
                                    { pitchState | ison = PitchState.NoIson }
                                )
                                model
                            , Task.attempt DomResult (Dom.focus "select-ison-button")
                            )

                        PitchState.Selected _ ->
                            ( model, Cmd.none )

                -- TODO: add in f and s for sharp/flat selection
                _ ->
                    ( model, Cmd.none )


updateAudioSettings : (AudioSettings -> AudioSettings) -> Model -> Model
updateAudioSettings f model =
    { model | audioSettings = f model.audioSettings }


updateLayoutData : (LayoutData -> LayoutData) -> Model -> Model
updateLayoutData f model =
    { model | layoutData = f model.layoutData }


updateModeSettings : (ModeSettings -> ModeSettings) -> Model -> Model
updateModeSettings f model =
    { model | modeSettings = f model.modeSettings }


setPitchState : PitchState -> Model -> Model
setPitchState pitchState model =
    { model | pitchState = pitchState }


updatePitchState : (PitchState -> PitchState) -> Model -> Model
updatePitchState f model =
    { model | pitchState = f model.pitchState }


focus : String -> Cmd Msg
focus id =
    Task.attempt DomResult (Dom.focus id)
