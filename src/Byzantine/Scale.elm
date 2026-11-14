module Byzantine.Scale exposing (Scale(..), all, decode, encode, name)

{-| Scales
-}


{-| Boyer calls the diatonic the "soft-diatonic scale", and the enharmonic,
"hard-diatonic".
-}
type Scale
    = SoftDiatonic
    | HardDiatonic
    | SoftChromatic
    | HardChromatic


all : List Scale
all =
    [ SoftDiatonic, HardDiatonic, SoftChromatic, HardChromatic ]


name : Scale -> String
name scale =
    case scale of
        SoftDiatonic ->
            "Soft Diatonic"

        HardDiatonic ->
            "Hard Diatonic (Enharmonic)"

        SoftChromatic ->
            "Soft Chromatic"

        HardChromatic ->
            "Hard Chromatic"


{-| Encode a Scale to a string representation:

  - "ds" for SoftDiatonic
  - "dh" for HardDiatonic
  - "cs" for SoftChromatic
  - "ch" for HardChromatic

-}
encode : Scale -> String
encode scale =
    case scale of
        SoftDiatonic ->
            "ds"

        HardDiatonic ->
            "dh"

        SoftChromatic ->
            "cs"

        HardChromatic ->
            "ch"


{-| Decode a string representation to a Scale. Valid inputs are:

  - "ds" for SoftDiatonic
  - "dh" for HardDiatonic
  - "cs" for SoftChromatic
  - "ch" for HardChromatic

Returns a Result with either the Scale or an error message if the input is invalid.

-}
decode : String -> Result String Scale
decode str =
    case str of
        "ds" ->
            Ok SoftDiatonic

        "dh" ->
            Ok HardDiatonic

        "cs" ->
            Ok SoftChromatic

        "ch" ->
            Ok HardChromatic

        _ ->
            Err ("Invalid scale format: " ++ str)
