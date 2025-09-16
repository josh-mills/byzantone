module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Byzantine.Frequency exposing (Frequency(..))
import Json.Decode as Decode
import Model exposing (Model)
import Model.DeviceInfo as DeviceInfo
import Ports
import Task
import Update exposing (Msg(..), update)
import View exposing (view)



-- MAIN


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    ( Decode.decodeValue DeviceInfo.decoder flags
        |> Result.withDefault DeviceInfo.default
        |> Model.initialModel
    , Task.perform GotViewport Dom.getViewport
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize ViewportResize
        , Ports.pitchDetected (\{ pitch } -> SetDetectedPitch (Maybe.map Frequency pitch))
        ]
