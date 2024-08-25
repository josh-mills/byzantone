module Byzantine.Scale exposing (Scale(..), all, name)

{-| Scales
-}


{-| Boyer calls the diatonic the "soft-diatonic scale", and the enharmonic,
"hard-diatonic".
-}
type Scale
    = Diatonic
    | Enharmonic
    | SoftChromatic
    | HardChromatic


all : List Scale
all =
    [ Diatonic, Enharmonic, SoftChromatic, HardChromatic ]


name : Scale -> String
name scale =
    case scale of
        Diatonic ->
            "Diatonic"

        Enharmonic ->
            "Enharmonic"

        SoftChromatic ->
            "Soft Chromatic"

        HardChromatic ->
            "Hard Chromatic"
