module Remote.AboutCopy exposing (AboutCopy, decoder, fetch)

import Http
import Json.Decode as Decode exposing (Decoder)


type alias AboutCopy =
    { content : String
    , lastUpdated : String
    }


decoder : Decoder AboutCopy
decoder =
    Decode.map2 AboutCopy
        (Decode.field "content" Decode.string)
        (Decode.field "lastUpdated" Decode.string)


fetch : (Result Http.Error AboutCopy -> msg) -> Cmd msg
fetch toMsg =
    Http.get
        { url = "dist/about.json"
        , expect = Http.expectJson toMsg decoder
        }
