module Icons exposing (xmark)

{-| TODO: this should be generated code. We should probably also consider the
API a bit; might want to add in additional attributes.
-}

import Html exposing (Html)
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, viewBox)


xmark : Html msg
xmark =
    svg [ viewBox "0 0 384 512" ]
        [ path
            [ d "M342.6 150.6c12.5-12.5 12.5-32.8 0-45.3s-32.8-12.5-45.3 0L192 210.7 86.6 105.4c-12.5-12.5-32.8-12.5-45.3 0s-12.5 32.8 0 45.3L146.7 256 41.4 361.4c-12.5 12.5-12.5 32.8 0 45.3s32.8 12.5 45.3 0L192 301.3 297.4 406.6c12.5 12.5 32.8 12.5 45.3 0s12.5-32.8 0-45.3L237.3 256 342.6 150.6z" ]
            []
        ]
