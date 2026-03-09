module Model.Changelog exposing (Changelog, Changes, Entry, decoder, fetch)

import Http
import Json.Decode as Decode exposing (Decoder)


type alias Changelog =
    { entries : List Entry
    , lastUpdated : String
    }


type alias Entry =
    { version : String
    , date : Maybe String
    , changes : Changes
    }


type alias Changes =
    { added : List String
    , changed : List String
    , deprecated : List String
    , removed : List String
    , fixed : List String
    , security : List String
    }


decoder : Decoder Changelog
decoder =
    Decode.map2 Changelog
        (Decode.field "entries" (Decode.list entryDecoder))
        (Decode.field "lastUpdated" Decode.string)


entryDecoder : Decoder Entry
entryDecoder =
    Decode.map3 Entry
        (Decode.field "version" Decode.string)
        (Decode.maybe (Decode.field "date" Decode.string))
        (Decode.field "changes" changesDecoder)


changesDecoder : Decoder Changes
changesDecoder =
    Decode.map6 Changes
        (Decode.field "added" (Decode.list Decode.string))
        (Decode.field "changed" (Decode.list Decode.string))
        (Decode.field "deprecated" (Decode.list Decode.string))
        (Decode.field "removed" (Decode.list Decode.string))
        (Decode.field "fixed" (Decode.list Decode.string))
        (Decode.field "security" (Decode.list Decode.string))


fetch : (Result Http.Error Changelog -> msg) -> Cmd msg
fetch toMsg =
    Http.get
        { url = "/dist/changelog.json"
        , expect = Http.expectJson toMsg decoder
        }
