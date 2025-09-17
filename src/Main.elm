module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Byzantine.Frequency exposing (Frequency(..))
import Json.Decode as Decode exposing (Decoder, Value)
import Model exposing (Model)
import Model.DeviceInfo as DeviceInfo exposing (DeviceInfo)
import Ports
import Task
import Update exposing (Msg(..), update)
import View exposing (view)



-- MAIN


main : Program Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Value -> ( Model, Cmd Msg )
init flags =
    let
        { deviceInfo, viewport } =
            Decode.decodeValue flagsDecoder flags
                |> Result.withDefault defaultFlags
    in
    ( Model.init deviceInfo viewport
    , Task.perform GotViewport Dom.getViewport
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize ViewportResize
        , Ports.pitchDetected (\{ pitch } -> SetDetectedPitch (Maybe.map Frequency pitch))
        ]



-- FLAGS


type alias Flags =
    { deviceInfo : DeviceInfo
    , viewport : { width : Float, height : Float }
    }


defaultFlags : Flags
defaultFlags =
    { deviceInfo = DeviceInfo.default
    , viewport = { width = 0, height = 256 }
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.map2 Flags
        (Decode.field "deviceInfo" DeviceInfo.decoder)
        (Decode.field "viewport" viewportDecoder)


viewportDecoder : Decoder { width : Float, height : Float }
viewportDecoder =
    Decode.map2 (\w h -> { width = w, height = h })
        (Decode.field "width" Decode.float)
        (Decode.field "height" Decode.float)
