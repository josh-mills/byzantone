module Byzantine.Mode.Classification exposing
    ( Classification, Division(..), Ordinal(..)
    , all, toString, basesFor
    )

{-| I think "Classification" (or maybe "OktoichosClassification") will work as a
term, at least for internal conceptual modeling. How this is exposed towards
users is a different question.

I'm wondering if it would be better to have just a flat list of eight rather than
a product type.


# Classification

@docs Classification, Division, Ordinal


## Functions

@docs all, toString, basesFor

-}

import Byzantine.Accidental exposing (Accidental(..))
import Byzantine.Degree exposing (Degree(..))
import Byzantine.Pitch as Pitch exposing (Pitch)
import Byzantine.Scale exposing (Scale(..))
import Maybe.Extra


type Classification
    = Classification Division Ordinal


type Division
    = Authentic
    | Plagal


type Ordinal
    = ModeOne
    | ModeTwo
    | ModeThree
    | ModeFour


all : List Classification
all =
    [ Classification Authentic ModeOne
    , Classification Authentic ModeTwo
    , Classification Authentic ModeThree
    , Classification Authentic ModeFour
    , Classification Plagal ModeOne
    , Classification Plagal ModeTwo
    , Classification Plagal ModeThree
    , Classification Plagal ModeFour
    ]


toString : Classification -> String
toString (Classification division ordinal) =
    case ( division, ordinal ) of
        ( Authentic, ModeOne ) ->
            "Authentic Mode One"

        ( Authentic, ModeTwo ) ->
            "Authentic Mode Two"

        ( Authentic, ModeThree ) ->
            "Authentic Mode Three"

        ( Authentic, ModeFour ) ->
            "Authentic Mode Four"

        ( Plagal, ModeOne ) ->
            "Plagal Mode One"

        ( Plagal, ModeTwo ) ->
            "Plagal Mode Two"

        ( Plagal, ModeThree ) ->
            "Grave Mode"

        ( Plagal, ModeFour ) ->
            "Plagal Mode Four"


{-| A Mode can be generally built from a classification plus a base.

A base is a pitch (which, in all but one case, will be natural, but
there does exist a grave variant on Ζω-flat, which prevents a simple
definition on degree).

But, not all pitches are actual bases.

Bases:

  - Authentic 1: Κε, Πα
  - Authentic 2: Δι, Πα -- Βου
  - Authentic 3: Γα
  - Authentic 4: Βου, Δι -- Πα, maybe?
  - Plagal 1: Κε, Πα
  - Plagal 2: Δι, Βου
  - Plagal 3: Γα, Ζω, Ζω-flat4
  - Plagal 4: Νη, Γα

-}
basesFor : Classification -> List Pitch
basesFor (Classification division ordinal) =
    case ( division, ordinal ) of
        ( Authentic, ModeOne ) ->
            [ Pitch.natural Ke, Pitch.natural Pa ]

        ( Authentic, ModeTwo ) ->
            [ Pitch.natural Di, Pitch.natural Pa ]

        ( Authentic, ModeThree ) ->
            [ Pitch.natural Ga ]

        ( Authentic, ModeFour ) ->
            [ Pitch.natural Bou, Pitch.natural Di ]

        ( Plagal, ModeOne ) ->
            [ Pitch.natural Ke, Pitch.natural Pa ]

        ( Plagal, ModeTwo ) ->
            [ Pitch.natural Di, Pitch.natural Bou ]

        ( Plagal, ModeThree ) ->
            [ Pitch.natural Ga |> Just
            , Pitch.natural Zo_ |> Just
            , Pitch.inflected Enharmonic Flat4 Zo_ |> Result.toMaybe
            ]
                |> Maybe.Extra.values

        ( Plagal, ModeFour ) ->
            [ Pitch.natural Ni, Pitch.natural Ga ]
