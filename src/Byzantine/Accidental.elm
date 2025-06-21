module Byzantine.Accidental exposing (Accidental(..), all, fromString, lower, moriaAdjustment, raise, toString)


type Accidental
    = Sharp2
    | Sharp4
    | Sharp6
    | Sharp8
    | Flat2
    | Flat4
    | Flat6
    | Flat8


{-| Lists all accidentals in order of inflection, from `Flat8` through
`Sharp8`.
-}
all : List Accidental
all =
    [ Flat8, Flat6, Flat4, Flat2, Sharp2, Sharp4, Sharp6, Sharp8 ]


raise : Accidental -> Maybe Accidental
raise accidental =
    case accidental of
        Sharp2 ->
            Just Sharp4

        Sharp4 ->
            Just Sharp6

        Sharp6 ->
            Just Sharp8

        Sharp8 ->
            Nothing

        Flat2 ->
            Nothing

        Flat4 ->
            Just Flat2

        Flat6 ->
            Just Flat4

        Flat8 ->
            Just Flat6


lower : Accidental -> Maybe Accidental
lower accidental =
    case accidental of
        Sharp2 ->
            Nothing

        Sharp4 ->
            Just Sharp2

        Sharp6 ->
            Just Sharp4

        Sharp8 ->
            Just Sharp6

        Flat2 ->
            Just Flat4

        Flat4 ->
            Just Flat6

        Flat6 ->
            Just Flat8

        Flat8 ->
            Nothing


moriaAdjustment : Accidental -> Int
moriaAdjustment accidental =
    case accidental of
        Sharp2 ->
            2

        Sharp4 ->
            4

        Sharp6 ->
            6

        Sharp8 ->
            8

        Flat2 ->
            -2

        Flat4 ->
            -4

        Flat6 ->
            -6

        Flat8 ->
            -8


toString : Accidental -> String
toString accidental =
    let
        val =
            moriaAdjustment accidental
    in
    if val > 0 then
        "+" ++ String.fromInt val

    else
        String.fromInt val


{-| Convert a string representation back to an Accidental. String must be in the
format "+N" or "-N" where N is a valid moria adjustment (2, 4, 6, or 8).
-}
fromString : String -> Result String Accidental
fromString str =
    case str of
        "+2" ->
            Ok Sharp2

        "+4" ->
            Ok Sharp4

        "+6" ->
            Ok Sharp6

        "+8" ->
            Ok Sharp8

        "-2" ->
            Ok Flat2

        "-4" ->
            Ok Flat4

        "-6" ->
            Ok Flat6

        "-8" ->
            Ok Flat8

        _ ->
            Err ("Invalid accidental: " ++ str)
