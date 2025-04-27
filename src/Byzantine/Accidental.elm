module Byzantine.Accidental exposing (Accidental(..), all, moriaAdjustment, toString)


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
