module Update exposing (Msg(..), update)

import Array
import Browser.Dom as Dom
import Byzantine.Accidental as Accidental exposing (Accidental)
import Byzantine.Degree as Degree exposing (Degree(..))
import Byzantine.Pitch as Pitch exposing (Pitch, PitchStandard, Register)
import Byzantine.Scale exposing (Scale)
import Maybe.Extra as Maybe
import Model exposing (Modal, Model)
import Model.AudioSettings exposing (AudioSettings)
import Model.LayoutData exposing (LayoutData, LayoutSelection)
import Model.ModeSettings exposing (ModeSettings)
import Model.PitchSpaceData as PitchSpaceData
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
    | PitchTrackerClicked String
    | SelectModal Modal
    | SelectPitch (Maybe Pitch) (Maybe Movement)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Task.perform GotViewport Dom.getViewport )

        PitchTrackerClicked _ ->
            -- this won't last.
            ( model
            , Cmd.none
            )

        DomResult _ ->
            -- just for dev purposes
            ( model, Cmd.none )

        GotPitchSpaceElement elementResult ->
            ( case elementResult of
                Ok element ->
                    updateLayoutData
                        (\layoutData -> { layoutData | pitchSpace = element })
                        model
                        |> resetPitchSpaceData

                Err _ ->
                    model
            , Cmd.none
            )

        GotViewport viewport ->
            ( updateLayoutData
                (\layoutData -> { layoutData | viewport = viewport })
                model
                |> resetPitchSpaceData
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

        SelectPitch maybePitch maybeMovement ->
            ( updatePitchState
                (\pitchState ->
                    { pitchState
                        | currentPitch = maybePitch
                        , proposedAccidental = Nothing
                        , proposedMovement =
                            if Maybe.isJust maybePitch then
                                Maybe.withDefault model.pitchState.proposedMovement maybeMovement

                            else
                                Movement.None
                    }
                )
                model
                |> resetPitchSpaceData
            , Cmd.none
            )

        SelectProposedAccidental maybeAccidental ->
            ( updatePitchState
                (\pitchState -> { pitchState | proposedAccidental = maybeAccidental })
                model
                |> resetPitchSpaceData
            , Cmd.none
            )

        SelectProposedMovement movement ->
            let
                movementWithProposedAccidental =
                    Movement.applyAccidental
                        model.modeSettings.scale
                        model.pitchState.proposedAccidental
                        movement
            in
            ( updatePitchState
                (\pitchState ->
                    { pitchState
                        | proposedMovement =
                            Maybe.unwrap Movement.None
                                (\currentPitch ->
                                    if
                                        Movement.isValid model.modeSettings.scale
                                            currentPitch
                                            movementWithProposedAccidental
                                    then
                                        movementWithProposedAccidental

                                    else
                                        movement
                                )
                                model.pitchState.currentPitch
                    }
                )
                model
                |> resetPitchSpaceData
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
                |> resetPitchSpaceData
            , Cmd.none
            )

        SetLayout layoutSelection ->
            ( updateLayoutData
                (\layoutData -> { layoutData | layoutSelection = layoutSelection })
                model
                |> resetPitchSpaceData
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
                |> resetPitchSpaceData
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
                |> resetPitchSpaceData
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
                |> resetPitchSpaceData
            , Cmd.none
            )

        ToggleMenu ->
            ( { model | menuOpen = not model.menuOpen }
            , if not model.menuOpen then
                focus "menu"

              else
                Cmd.none
            )

        Keydown key ->
            case key of
                "ArrowUp" ->
                    moveAndFocus model 1

                "ArrowDown" ->
                    moveAndFocus model -1

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
                    moveAndFocus model 1

                "2" ->
                    moveAndFocus model 2

                "3" ->
                    moveAndFocus model 3

                "4" ->
                    moveAndFocus model 4

                "5" ->
                    moveAndFocus model 5

                "6" ->
                    moveAndFocus model 6

                "7" ->
                    moveAndFocus model 7

                "8" ->
                    moveAndFocus model 8

                "9" ->
                    moveAndFocus model 9

                "!" ->
                    moveAndFocus model -1

                "@" ->
                    moveAndFocus model -2

                "#" ->
                    moveAndFocus model -3

                "$" ->
                    moveAndFocus model -4

                "%" ->
                    moveAndFocus model -5

                "^" ->
                    moveAndFocus model -6

                "&" ->
                    moveAndFocus model -7

                "*" ->
                    moveAndFocus model -8

                "(" ->
                    moveAndFocus model -9

                "n" ->
                    setAndFocus model Ni

                "p" ->
                    setAndFocus model Pa

                "b" ->
                    setAndFocus model Bou

                "v" ->
                    setAndFocus model Bou

                "g" ->
                    setAndFocus model Ga

                "d" ->
                    setAndFocus model Di

                "k" ->
                    setAndFocus model Ke

                "z" ->
                    setAndFocus model Zo_

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

                "f" ->
                    ( updatePitchState
                        (\pitchState ->
                            { pitchState
                                | proposedAccidental =
                                    case pitchState.proposedAccidental of
                                        Just accidental ->
                                            Accidental.lower accidental

                                        Nothing ->
                                            Just Accidental.Flat2
                            }
                        )
                        model
                    , Cmd.none
                    )

                "s" ->
                    ( updatePitchState
                        (\pitchState ->
                            { pitchState
                                | proposedAccidental =
                                    case pitchState.proposedAccidental of
                                        Just accidental ->
                                            Accidental.raise accidental

                                        Nothing ->
                                            Just Accidental.Sharp2
                            }
                        )
                        model
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


resetPitchSpaceData : Model -> Model
resetPitchSpaceData model =
    { model
        | pitchSpaceData =
            PitchSpaceData.init
                model.layoutData
                model.modeSettings
                model.pitchState
    }


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



-- KEYBOARD SHORTCUT HELPERS


moveAndFocus : Model -> Int -> ( Model, Cmd Msg )
moveAndFocus model interval =
    let
        pitch =
            model.pitchState.currentPitch
                |> Maybe.map Pitch.unwrapDegree
                |> Maybe.andThen (\degree -> Degree.step degree interval)
                |> Maybe.map (Pitch.from model.modeSettings.scale model.pitchState.proposedAccidental)
    in
    ( updatePitchState
        (\pitchState ->
            { pitchState
                | currentPitch = pitch
                , proposedAccidental = Nothing
            }
        )
        model
        |> resetPitchSpaceData
    , Maybe.unwrap Cmd.none
        (\pitch_ ->
            Pitch.unwrapDegree pitch_
                |> Degree.toString
                |> (++) "p_"
                |> Dom.focus
                |> Task.attempt DomResult
        )
        pitch
    )


setAndFocus : Model -> Degree -> ( Model, Cmd Msg )
setAndFocus model degree =
    ( case model.pitchState.ison of
        PitchState.SelectingIson _ ->
            updatePitchState
                (\pitchState -> { pitchState | ison = PitchState.Selected degree })
                model
                |> resetPitchSpaceData

        _ ->
            updatePitchState
                (\pitchState ->
                    { pitchState
                        | currentPitch =
                            Just
                                (Pitch.from
                                    model.modeSettings.scale
                                    pitchState.proposedAccidental
                                    degree
                                )
                        , proposedAccidental = Nothing
                    }
                )
                model
                |> resetPitchSpaceData
    , Degree.toString degree |> (++) "p_" |> Dom.focus |> Task.attempt DomResult
    )



-- CMD HELPERS


focus : String -> Cmd Msg
focus id =
    Task.attempt DomResult (Dom.focus id)
