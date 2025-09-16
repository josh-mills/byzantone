module Model.DeviceInfo exposing (DeviceInfo, decoder, default)

import Json.Decode as Decode exposing (Decoder)


type alias DeviceInfo =
    { userAgent : String
    , isMobileDevice : Bool
    , hasTouchScreen : Bool
    }


decoder : Decoder DeviceInfo
decoder =
    Decode.map3 DeviceInfo
        (Decode.field "userAgent" Decode.string)
        (Decode.field "isMobileDevice" Decode.bool)
        (Decode.field "hasTouchScreen" Decode.bool)


default : DeviceInfo
default =
    { userAgent = "Unknown"
    , isMobileDevice = False
    , hasTouchScreen = False
    }
