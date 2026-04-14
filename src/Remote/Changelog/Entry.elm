module Remote.Changelog.Entry exposing (Changes, Entry, compare, decoder)

import Date exposing (Date)
import Json.Decode as Decode exposing (Decoder)
import Remote.Changelog.Version as Version exposing (Version)


type alias Entry =
    { version : Version
    , date : Date
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


compare : Entry -> Entry -> Order
compare a b =
    Version.compareVersion a.version b.version


decoder : Decoder Entry
decoder =
    Decode.map3 Entry
        (Decode.field "version" Version.decoder)
        (Decode.field "date" dateDecoder)
        (Decode.field "changes" changesDecoder)


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


changesDecoder : Decoder Changes
changesDecoder =
    Decode.map6 Changes
        (Decode.field "added" (Decode.list Decode.string))
        (Decode.field "changed" (Decode.list Decode.string))
        (Decode.field "deprecated" (Decode.list Decode.string))
        (Decode.field "removed" (Decode.list Decode.string))
        (Decode.field "fixed" (Decode.list Decode.string))
        (Decode.field "security" (Decode.list Decode.string))
