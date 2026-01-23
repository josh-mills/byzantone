module Update exposing (Msg(..), update)

import Array
import Browser.Dom as Dom
import Byzantine.Accidental as Accidental exposing (Accidental)
import Byzantine.Degree as Degree exposing (Degree(..))
import Byzantine.Frequency exposing (Frequency, PitchStandard)
import Byzantine.Pitch as Pitch exposing (Pitch)
import Byzantine.PitchPosition as PitchPosition
import Byzantine.Register exposing (Register)
import Byzantine.Scale exposing (Scale)
import Maybe.Extra as Maybe
import Model exposing (Modal, Model)
import Model.AudioSettings as AudioSettings exposing (AudioSettings)
import Model.ControlsMenu as ControlsMenu
import Model.DegreeDataDict as DegreeDataDict exposing (DegreeDataDict)
import Model.LayoutData exposing (LayoutData, LayoutSelection)
import Model.ModeSettings exposing (ModeSettings)
import Model.PitchSpaceData as PitchSpaceData exposing (PitchSpaceData)
import Model.PitchState as PitchState exposing (IsonStatus(..), PitchState, ProposedAccidental(..))
import Movement exposing (Movement)
import Task


type Msg
    = CloseControlMenus
    | DomResult (Result Dom.Error ())
    | GotPitchSpaceElement (Result Dom.Error Dom.Element)
    | GotViewport Dom.Viewport
    | ViewportResize Int Int
    | Keydown String
    | NoOp
    | PitchButtonClicked Degree
    | SelectModal Modal
    | SelectPitch (Maybe Pitch) (Maybe Movement)
    | SelectProposedAccidental ProposedAccidental
    | SelectProposedMovement Movement
    | SetAudioMode AudioSettings.AudioMode
    | SetDetectedPitch (Maybe Frequency)
    | SetGain Float
    | SetIson IsonStatus
    | SetLayout LayoutSelection
    | SetPitchStandard PitchStandard
    | SetRangeStart String
    | SetRangeEnd String
    | SetPlaybackRegister Register
    | SetListenRegister Register
    | SetResponsiveness AudioSettings.Responsiveness
    | SetScale Scale
    | ToggleControlMenu ControlsMenu.MenuOption
    | ToggleMenu


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Task.perform GotViewport Dom.getViewport )

        CloseControlMenus ->
            ( { model | openControlMenus = ControlsMenu.init }
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

        PitchButtonClicked degree ->
            ( processPitchButtonClick model degree
            , Cmd.none
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
                        | currentDegree = Maybe.map Pitch.unwrapDegree maybePitch
                        , appliedAccidentals =
                            case ( maybePitch, pitchState.currentDegree ) of
                                ( Just newPitch, Just currentDegree ) ->
                                    pitchState.appliedAccidentals
                                        |> DegreeDataDict.set (Pitch.unwrapDegree newPitch)
                                            (Pitch.unwrapAccidental newPitch)
                                        |> DegreeDataDict.set currentDegree Nothing

                                ( Just newPitch, Nothing ) ->
                                    DegreeDataDict.set (Pitch.unwrapDegree newPitch)
                                        (Pitch.unwrapAccidental newPitch)
                                        pitchState.appliedAccidentals

                                ( Nothing, _ ) ->
                                    pitchState.appliedAccidentals
                        , proposedAccidental = NoProposedAccidental
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

        SelectProposedAccidental proposedAccidental ->
            ( updatePitchState
                (\pitchState -> { pitchState | proposedAccidental = proposedAccidental })
                model
                |> resetPitchSpaceData
            , Cmd.none
            )

        SelectProposedMovement movement ->
            let
                movementWithProposedAccidental =
                    Movement.applyAccidental
                        model.modeSettings.scale
                        (PitchState.unwrapProposedAccidental model.pitchState.proposedAccidental)
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
                                (PitchState.currentPitch model.modeSettings.scale pitchState)
                    }
                )
                model
                |> resetPitchSpaceData
            , Cmd.none
            )

        SetAudioMode mode ->
            ( { model | pitchState = PitchState.initialPitchState }
                |> updateAudioSettings (\audioSettings -> { audioSettings | audioMode = mode })
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

        SetPlaybackRegister register ->
            ( updateAudioSettings
                (\audioSettings -> { audioSettings | playbackRegister = register })
                model
            , Cmd.none
            )

        SetListenRegister register ->
            ( updateAudioSettings
                (\audioSettings -> { audioSettings | listenRegister = register })
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

        SetResponsiveness responsiveness ->
            ( updateAudioSettings
                (\audioSettings -> { audioSettings | responsiveness = responsiveness })
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

        SetDetectedPitch pitchFrequency ->
            ( { model | detectedPitch = pitchFrequency }
            , Cmd.none
            )

        ToggleControlMenu menuOption ->
            ( { model
                | openControlMenus =
                    ControlsMenu.toggle menuOption model.openControlMenus
              }
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
                            |> resetPitchSpaceData
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
                                        Apply accidental ->
                                            case Accidental.lower accidental of
                                                Just lowerAccidental ->
                                                    Apply lowerAccidental

                                                Nothing ->
                                                    CancelAccidental

                                        _ ->
                                            Apply Accidental.Flat2
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
                                        Apply accidental ->
                                            case Accidental.raise accidental of
                                                Just raisedAccidental ->
                                                    Apply raisedAccidental

                                                Nothing ->
                                                    CancelAccidental

                                        _ ->
                                            Apply Accidental.Sharp2
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


{-| Clicking the pitch button has two different core behaviors, depending on the
audio mode.

If in Play mode, this is the main means of interacting with the playback engine:
it will select or deselect the pitch, apply or de-apply accidentals, or set the
ison.

If in Listen mode, this is used only for setting the accidental.

-}
processPitchButtonClick : Model -> Degree -> Model
processPitchButtonClick model degree =
    case model.audioSettings.audioMode of
        AudioSettings.Listen ->
            updatePitchState
                (\pitchState ->
                    { pitchState
                        | appliedAccidentals =
                            applyAccidentalWithValidation model.modeSettings.scale
                                model.pitchSpaceData
                                model.pitchState
                                degree
                        , proposedAccidental = NoProposedAccidental
                    }
                )
                model
                |> resetPitchSpaceData

        AudioSettings.Play ->
            let
                isCurrentDegree =
                    Just degree == model.pitchState.currentDegree

                -- depending on how this goes, we might want to pull this whole thing
                -- into the PitchState module.
                ( newIson, newCurrentDegree, newAppliedAccidentals ) =
                    case
                        ( model.pitchState.ison
                        , model.pitchState.proposedAccidental
                        , isCurrentDegree
                        )
                    of
                        ( SelectingIson _, _, _ ) ->
                            ( Selected degree
                            , model.pitchState.currentDegree
                            , DegreeDataDict.set degree
                                Nothing
                                model.pitchState.appliedAccidentals
                            )

                        ( ison, CancelAccidental, _ ) ->
                            ( ison
                            , model.pitchState.currentDegree
                            , DegreeDataDict.set degree
                                Nothing
                                model.pitchState.appliedAccidentals
                            )

                        ( ison, Apply _, True ) ->
                            ( ison
                            , model.pitchState.currentDegree
                            , applyAccidentalWithValidation model.modeSettings.scale
                                model.pitchSpaceData
                                model.pitchState
                                degree
                            )

                        ( ison, NoProposedAccidental, True ) ->
                            ( ison
                            , Nothing
                            , DegreeDataDict.set degree
                                Nothing
                                model.pitchState.appliedAccidentals
                            )

                        ( ison, Apply _, False ) ->
                            ( ison
                            , Just degree
                            , applyAccidentalWithValidation model.modeSettings.scale
                                model.pitchSpaceData
                                model.pitchState
                                degree
                                |> clearCurrentDegreeAccidental model.pitchState.currentDegree
                            )

                        _ ->
                            ( model.pitchState.ison
                            , if model.pitchState.currentDegree == Just degree then
                                Nothing

                              else
                                Just degree
                            , clearCurrentDegreeAccidental
                                model.pitchState.currentDegree
                                model.pitchState.appliedAccidentals
                            )
            in
            updatePitchState
                (\pitchState ->
                    { pitchState
                        | currentDegree = newCurrentDegree
                        , ison = newIson
                        , appliedAccidentals = newAppliedAccidentals
                        , proposedAccidental = NoProposedAccidental
                    }
                )
                model
                |> resetPitchSpaceData


applyAccidentalWithValidation : Scale -> PitchSpaceData -> PitchState -> Degree -> DegreeDataDict (Maybe Accidental)
applyAccidentalWithValidation scale { pitchPositions } pitchState degree =
    case pitchState.proposedAccidental of
        Apply accidental ->
            let
                isValidInflection =
                    PitchPosition.isValidInflection scale accidental degree

                positonWithInflection =
                    PitchPosition.inflect (DegreeDataDict.get degree pitchPositions) accidental

                wouldNotBeInversion =
                    if Accidental.moriaAdjustment accidental > 0 then
                        Maybe.unwrap False
                            (\degreeHigher ->
                                PitchPosition.compare
                                    (DegreeDataDict.get degreeHigher pitchPositions)
                                    positonWithInflection
                                    == GT
                            )
                            (Degree.step degree 1)

                    else
                        Maybe.unwrap False
                            (\degreeLower ->
                                PitchPosition.compare
                                    (DegreeDataDict.get degreeLower pitchPositions)
                                    positonWithInflection
                                    == LT
                            )
                            (Degree.step degree -1)
            in
            if isValidInflection && wouldNotBeInversion then
                DegreeDataDict.set degree (Just accidental) pitchState.appliedAccidentals

            else
                pitchState.appliedAccidentals

        CancelAccidental ->
            DegreeDataDict.set degree Nothing pitchState.appliedAccidentals

        NoProposedAccidental ->
            if Maybe.isJust (DegreeDataDict.get degree pitchState.appliedAccidentals) then
                DegreeDataDict.set degree Nothing pitchState.appliedAccidentals

            else
                pitchState.appliedAccidentals


clearCurrentDegreeAccidental : Maybe Degree -> DegreeDataDict (Maybe Accidental) -> DegreeDataDict (Maybe Accidental)
clearCurrentDegreeAccidental currentDegree appliedAccidentals =
    case currentDegree of
        Just degree ->
            DegreeDataDict.set degree
                Nothing
                appliedAccidentals

        Nothing ->
            appliedAccidentals



-- KEYBOARD SHORTCUT HELPERS


moveAndFocus : Model -> Int -> ( Model, Cmd Msg )
moveAndFocus model interval =
    let
        pitch =
            model.pitchState.currentDegree
                |> Maybe.andThen (\degree -> Degree.step degree interval)
                |> Maybe.map
                    (Pitch.from model.modeSettings.scale
                        (PitchState.unwrapProposedAccidental model.pitchState.proposedAccidental)
                    )
    in
    ( updatePitchState
        (\pitchState ->
            { pitchState
                | currentDegree = Maybe.map Pitch.unwrapDegree pitch
                , proposedAccidental = NoProposedAccidental
                , appliedAccidentals =
                    clearCurrentDegreeAccidental pitchState.currentDegree
                        pitchState.appliedAccidentals
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
                        | currentDegree = Just degree
                        , proposedAccidental = NoProposedAccidental
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
