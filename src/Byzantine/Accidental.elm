module Byzantine.Accidental exposing
    ( Accidental(..), all, allFlats, allSharps
    , raiseWithin, lowerWithin
    , moriaAdjustment, toString, fromString
    )

{-| Accidentals: the sharps and flats that inflect a pitch, each by a number of
moria.

Data modeling gap: it may be better to create a distinction between
permament and transient accidentals. Permament are currently treated as
fthora in the view.


# Accidental

@docs Accidental, all, allFlats, allSharps


# Stepping

@docs raiseWithin, lowerWithin


# Conversion

@docs moriaAdjustment, toString, fromString

-}

import List.Extra


{-| A sharp (raises a pitch) or flat (lowers a pitch), named for its size in
moria.
-}
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


{-| Lists all flat accidentals in order from most flat to least flat.
-}
allFlats : List Accidental
allFlats =
    [ Flat8, Flat6, Flat4, Flat2 ]


{-| Lists all sharp accidentals in order from least sharp to most sharp.
-}
allSharps : List Accidental
allSharps =
    [ Sharp2, Sharp4, Sharp6, Sharp8 ]


{-| The nearest of the candidates that is higher than the given accidental,
without crossing natural: a flat only raises to a less-flat candidate, never to a
sharp. `Nothing` stands for natural, so it raises to the least sharp candidate.
Candidates need not be sorted.
-}
raiseWithin : List Accidental -> Maybe Accidental -> Maybe Accidental
raiseWithin candidates from =
    stepWithin 1 candidates from


{-| The nearest of the candidates that is lower than the given accidental,
without crossing natural: a sharp only lowers to a less-sharp candidate, never to
a flat. `Nothing` stands for natural, so it lowers to the least flat candidate.
Candidates need not be sorted.
-}
lowerWithin : List Accidental -> Maybe Accidental -> Maybe Accidental
lowerWithin candidates from =
    stepWithin -1 candidates from


{-| Step in a direction: `1` is up (toward sharper), `-1` is down.
-}
stepWithin : Int -> List Accidental -> Maybe Accidental -> Maybe Accidental
stepWithin direction candidates from =
    let
        start =
            Maybe.map moriaAdjustment from |> Maybe.withDefault 0

        isAhead candidate =
            let
                adjustment =
                    moriaAdjustment candidate
            in
            direction * (adjustment - start) > 0 && (start == 0 || (adjustment < 0) == (start < 0))
    in
    candidates
        |> List.filter isAhead
        |> List.Extra.minimumBy (\candidate -> direction * moriaAdjustment candidate)


{-| The signed number of moria an accidental adjusts a pitch by: positive for
sharps, negative for flats.
-}
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


{-| The moria adjustment as a signed string, e.g., `"+2"` or `"-4"`. The inverse
of `fromString`.
-}
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
