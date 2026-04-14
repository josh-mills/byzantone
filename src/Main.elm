module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Byzantine.Frequency exposing (Frequency(..))
import Date exposing (Date)
import Json.Decode as Decode exposing (Decoder, Value)
import Model exposing (Model)
import Model.DeviceInfo as DeviceInfo exposing (DeviceInfo)
import Ports
import Task
import Time
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
        { deviceInfo, viewport, currentDate } =
            Decode.decodeValue flagsDecoder flags
                |> Result.withDefault defaultFlags
    in
    ( Model.init deviceInfo viewport currentDate
    , Task.perform GotViewport Dom.getViewport
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize ViewportResize
        , Ports.pitchDetected
            (\{ pitch, timestamp } ->
                SetDetectedPitch
                    { maybeFrequency = Maybe.map Frequency pitch
                    , timestamp = Time.millisToPosix timestamp
                    }
            )
        ]



-- FLAGS


type alias Flags =
    { deviceInfo : DeviceInfo
    , viewport : { width : Float, height : Float }
    , currentDate : Maybe Date
    }


defaultFlags : Flags
defaultFlags =
    { deviceInfo = DeviceInfo.default
    , viewport = { width = 0, height = 256 }
    , currentDate = Nothing
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.map3 Flags
        (Decode.field "deviceInfo" DeviceInfo.decoder)
        (Decode.field "viewport" viewportDecoder)
        (Decode.field "currentDate" dateDecoder)


dateDecoder : Decoder (Maybe Date)
dateDecoder =
    Decode.string
        |> Decode.map
            (\str ->
                case Date.fromIsoString (String.left 10 str) of
                    Ok date ->
                        Just date

                    Err _ ->
                        Nothing
            )


viewportDecoder : Decoder { width : Float, height : Float }
viewportDecoder =
    Decode.map2 (\w h -> { width = w, height = h })
        (Decode.field "width" Decode.float)
        (Decode.field "height" Decode.float)
