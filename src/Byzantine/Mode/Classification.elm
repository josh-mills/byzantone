module Byzantine.Mode.Classification exposing
    ( Classification(..), Ordinal(..)
    , all, toString, basesFor, ordinal
    )

{-| `Classification` represents the Oktoichos (Ὀκτώηχος) classification: four
authentic modes and four plagal modes. But, this system is primarily a
classification system for liturgical purposes, so the actual musical structure
of the modes is more complex.


# Classification

@docs Classification, Ordinal


## Functions

@docs all, toString, basesFor, ordinal

-}

import Byzantine.Accidental exposing (Accidental(..))
import Byzantine.Degree exposing (Degree(..))
import Byzantine.Pitch as Pitch exposing (Pitch)
import Byzantine.Scale exposing (Scale(..))
import Maybe.Extra


type Classification
    = Authentic Ordinal
    | Plagal Ordinal


type Ordinal
    = ModeOne
    | ModeTwo
    | ModeThree
    | ModeFour


ordinal : Classification -> Ordinal
ordinal classification =
    case classification of
        Authentic o ->
            o

        Plagal o ->
            o


all : List Classification
all =
    [ Authentic ModeOne
    , Authentic ModeTwo
    , Authentic ModeThree
    , Authentic ModeFour
    , Plagal ModeOne
    , Plagal ModeTwo
    , Plagal ModeThree
    , Plagal ModeFour
    ]


toString : Classification -> String
toString classification =
    case classification of
        Authentic ModeOne ->
            "First Mode"

        Authentic ModeTwo ->
            "Second Mode"

        Authentic ModeThree ->
            "Third Mode"

        Authentic ModeFour ->
            "Fourth Mode"

        Plagal ModeOne ->
            "Plagal First Mode"

        Plagal ModeTwo ->
            "Plagal Second Mode"

        Plagal ModeThree ->
            "Grave Mode"

        Plagal ModeFour ->
            "Plagal Fourth Mode"


{-| A Mode can be generally built from a classification plus a base.

A base is a pitch (which, in all but one case, will be natural, but there does
exist a grave variant on Ζω-flat, which prevents a simple definition on degree).

But, not all pitches are actual bases.

Bases:

  - Authentic 1: Κε, Πα
  - Authentic 2: Δι, Πα, Βου
  - Authentic 3: Γα
  - Authentic 4: Βου, Δι, Πα
  - Plagal 1: Κε, Πα
  - Plagal 2: Δι, Βου, Πα
  - Plagal 3: Γα, Ζω, Ζω-flat4
  - Plagal 4: Νη, Γα

It may be worth it to make a separate Mode.Base module, but that would have to
be kept separate from the concept of the modal signature base.

-}
basesFor : Classification -> List Pitch
basesFor classification =
    case classification of
        Authentic ModeOne ->
            [ Pitch.natural Ke, Pitch.natural Pa ]

        Authentic ModeTwo ->
            [ Pitch.natural Di, Pitch.natural Pa, Pitch.natural Bou ]

        Authentic ModeThree ->
            [ Pitch.natural Ga ]

        Authentic ModeFour ->
            [ Pitch.natural Bou, Pitch.natural Di, Pitch.natural Pa ]

        Plagal ModeOne ->
            [ Pitch.natural Ke, Pitch.natural Pa ]

        Plagal ModeTwo ->
            [ Pitch.natural Di, Pitch.natural Bou, Pitch.natural Pa ]

        Plagal ModeThree ->
            [ Pitch.natural Ga |> Just
            , Pitch.natural Zo_ |> Just
            , Pitch.inflected Enharmonic Flat4 Zo_ |> Result.toMaybe
            ]
                |> Maybe.Extra.values

        Plagal ModeFour ->
            [ Pitch.natural Ni, Pitch.natural Ga ]
