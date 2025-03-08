module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Model exposing (Model)
import Task
import Update exposing (Msg(..), update)
import View exposing (view)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always (Browser.Events.onResize ViewportResize)
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model.initialModel
    , Cmd.batch
        [ Task.perform GotViewport Dom.getViewport
        , Task.attempt GotViewportOfPitchSpace (Dom.getViewportOf "pitch-space")
        ]
    )
