module AudioSettings exposing (AudioSettings, Register(..), adjustPitch)


type alias AudioSettings =
    { gain : Float
    , register : Register
    }


type Register
    = Treble
    | Bass


adjustPitch : Register -> Float -> Float
adjustPitch register pitch =
    case register of
        Treble ->
            pitch

        Bass ->
            pitch / 2
