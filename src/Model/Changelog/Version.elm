module Model.Changelog.Version exposing (Version, compareVersion, decoder)

import Json.Decode as Decode exposing (Decoder)


type alias Version =
    { major : Int
    , minor : Int
    , patch : Int
    }


compareVersion : Version -> Version -> Order
compareVersion a b =
    case compare a.major b.major of
        EQ ->
            case compare a.minor b.minor of
                EQ ->
                    compare a.patch b.patch

                order ->
                    order

        order ->
            order


decoder : Decoder Version
decoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str |> String.split "." |> List.map String.toInt of
                    [ Just major, Just minor, Just patch ] ->
                        Decode.succeed { major = major, minor = minor, patch = patch }

                    _ ->
                        Decode.fail ("Invalid version format: " ++ str)
            )
