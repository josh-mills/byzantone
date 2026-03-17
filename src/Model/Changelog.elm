module Model.Changelog exposing (Changelog, decoder, fetch)

import Http
import Json.Decode as Decode exposing (Decoder)
import Model.Changelog.Entry as Entry exposing (Entry)


type alias Changelog =
    { entries : List Entry
    , lastUpdated : String
    }


lenientList : Decoder a -> Decoder (List a)
lenientList itemDecoder =
    Decode.list (Decode.maybe itemDecoder)
        |> Decode.map (List.filterMap identity)


decoder : Decoder Changelog
decoder =
    Decode.map2 Changelog
        (Decode.field "entries"
            (lenientList Entry.decoder
                |> Decode.map (List.sortWith (\a b -> Entry.compare b a))
            )
        )
        (Decode.field "lastUpdated" Decode.string)


fetch : (Result Http.Error Changelog -> msg) -> Cmd msg
fetch toMsg =
    Http.get
        { url = "dist/changelog.json"
        , expect = Http.expectJson toMsg decoder
        }
