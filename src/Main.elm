module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Byzantine.Frequency exposing (Frequency(..))
import Model exposing (Model)
import Ports
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
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model.initialModel
    , Task.perform GotViewport Dom.getViewport
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize ViewportResize
        , Ports.pitchDetected (\{ pitch } -> SetDetectedPitch (Maybe.map Frequency pitch))
        ]
