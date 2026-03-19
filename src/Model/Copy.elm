module Model.Copy exposing (Copy, decoder, fetch)

import Http
import Json.Decode as Decode exposing (Decoder)


type alias Copy =
    { content : String
    , lastUpdated : String
    }


decoder : Decoder Copy
decoder =
    Decode.map2 Copy
        (Decode.field "content" Decode.string)
        (Decode.field "lastUpdated" Decode.string)


fetch : (Result Http.Error Copy -> msg) -> Cmd msg
fetch toMsg =
    Http.get
        { url = "dist/about.json"
        , expect = Http.expectJson toMsg decoder
        }
