module Model.Changelog exposing (Changelog, Changes, Entry, Version, decoder, fetch)

import Date exposing (Date)
import Http
import Json.Decode as Decode exposing (Decoder)


type alias Changelog =
    { entries : List Entry
    , lastUpdated : String
    }


type alias Entry =
    { version : Version
    , date : Date
    , changes : Changes
    }


type alias Version =
    { major : Int
    , minor : Int
    , patch : Int
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


versionDecoder : Decoder Version
versionDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str |> String.split "." |> List.map String.toInt of
                    [ Just major, Just minor, Just patch ] ->
                        Decode.succeed { major = major, minor = minor, patch = patch }

                    _ ->
                        Decode.fail ("Invalid version format: " ++ str)
            )


dateDecoder : Decoder Date
dateDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case Date.fromIsoString str of
                    Ok date ->
                        Decode.succeed date

                    Err err ->
                        Decode.fail ("Invalid ISO-8601 date: " ++ err)
            )


entryDecoder : Decoder Entry
entryDecoder =
    Decode.map3 Entry
        (Decode.field "version" versionDecoder)
        (Decode.field "date" dateDecoder)
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
