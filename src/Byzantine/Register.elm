module Byzantine.Register exposing (Register(..), factor, toString)


type Register
    = Treble
    | Bass


toString : Register -> String
toString register =
    case register of
        Treble ->
            "Treble"

        Bass ->
            "Bass"


{-| Factor by which to adjust a frequency based on register
-}
factor : Register -> Float
factor register =
    case register of
        Treble ->
            1.0

        Bass ->
            0.5
