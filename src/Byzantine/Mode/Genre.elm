module Byzantine.Mode.Genre exposing (..)


type Genre
    = Eirmologic
    | Sticheraric
    | Papadic


toString : Genre -> String
toString genre =
    case genre of
        Eirmologic ->
            "Eirmologic"

        Sticheraric ->
            "Sticheraric"

        Papadic ->
            "Papadic"
