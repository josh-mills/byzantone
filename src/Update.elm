module Update exposing (Msg(..), update)

import AudioSettings exposing (Register)
import Browser.Dom as Dom
import Byzantine.Degree as Degree exposing (Degree(..))
import Byzantine.Scale exposing (Scale)
import Maybe.Extra as Maybe
import Model exposing (Modal, Model)
import Movement exposing (Movement)
import Platform.Cmd as Cmd
import Task


type Msg
    = DomResult (Result Dom.Error ())
    | GotViewport Dom.Viewport
    | ViewportResize Int Int
    | Keydown String
    | NoOp
    | SelectModal Modal
    | SelectPitch (Maybe Degree) (Maybe Movement)
    | SelectProposedMovement Movement
    | SetGain Float
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

        GotViewport viewport ->
            ( { model | viewport = viewport }
            , Cmd.none
            )

        ViewportResize _ _ ->
            ( model
            , Task.perform GotViewport Dom.getViewport
            )

        SelectModal modal ->
            ( { model | modal = modal, menuOpen = False }
            , if Model.modalOpen modal then
                focus "modal"

              else
                Cmd.none
            )

        SelectPitch pitch maybeMovement ->
            ( { model
                | currentPitch = pitch
                , proposedMovement = Maybe.withDefault model.proposedMovement maybeMovement
              }
            , Cmd.none
            )

        SelectProposedMovement movement_ ->
            ( { model | proposedMovement = movement_ }
            , Cmd.none
            )

        SetGain gain ->
            let
                audioSettings =
                    model.audioSettings
            in
            ( { model | audioSettings = { audioSettings | gain = clamp 0 1 gain } }
            , Cmd.none
            )

        SetRegister register ->
            let
                audioSettings =
                    model.audioSettings
            in
            ( { model | audioSettings = { audioSettings | register = register } }
            , Cmd.none
            )

        SetScale scale ->
            ( { model
                | scale = scale

                -- , currentPitch = Nothing -- consider this.
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

        ToggleSpacing ->
            ( { model | showSpacing = not model.showSpacing }
            , Cmd.none
            )

        Keydown key ->
            let
                moveAndFocus i =
                    let
                        degree =
                            Maybe.map (\d -> Degree.step d i |> Maybe.withDefault d) model.currentPitch
                    in
                    ( { model | currentPitch = degree }
                    , Maybe.unwrap Cmd.none
                        (Degree.toString >> (++) "p_" >> Dom.focus >> Task.attempt DomResult)
                        degree
                    )

                setAndFocus d =
                    ( { model | currentPitch = Just d }
                    , Degree.toString d |> (++) "p_" |> Dom.focus |> Task.attempt DomResult
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
                        ( { model | currentPitch = Nothing }
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

                _ ->
                    ( model, Cmd.none )


focus : String -> Cmd Msg
focus id =
    Task.attempt DomResult (Dom.focus id)
