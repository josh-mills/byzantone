module Byzantine.ByzHtml.Accidental exposing (view)

import Byzantine.Accidental exposing (Accidental(..))
import Html exposing (Html, node)


{-| TODO: in the byzhtml spec, there are additional placement options that
are not currently encoded here.

<https://danielgarthur.github.io/byzhtml/#/component-list-neumes>

-}
view : Accidental -> Html msg
view accidental =
    case accidental of
        Sharp2 ->
            node "x-diesis-2" [] []

        Sharp4 ->
            node "x-diesis-4" [] []

        Sharp6 ->
            node "x-diesis-6" [] []

        Sharp8 ->
            node "x-diesis-8" [] []

        Flat2 ->
            node "x-yfesis-2" [] []

        Flat4 ->
            node "x-yfesis-4" [] []

        Flat6 ->
            node "x-yfesis-6" [] []

        Flat8 ->
            node "x-yfesis-8" [] []
