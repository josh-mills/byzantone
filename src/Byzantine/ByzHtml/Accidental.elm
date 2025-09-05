module Byzantine.ByzHtml.Accidental exposing (Color(..), view)

import Byzantine.Accidental exposing (Accidental(..))
import Html exposing (Html, node)
import Html.Attributes exposing (class)


{-| TODO: in the byzhtml spec, there are additional placement options that
are not currently encoded here.

<https://danielgarthur.github.io/byzhtml/#/component-list-neumes>

-}
view : Color -> Accidental -> Html msg
view color accidental =
    let
        attr =
            case color of
                InheritColor ->
                    []

                Red ->
                    [ class "text-red-700" ]
    in
    case accidental of
        Sharp2 ->
            node "x-diesis-2" attr []

        Sharp4 ->
            node "x-diesis-4" attr []

        Sharp6 ->
            node "x-diesis-6" attr []

        Sharp8 ->
            node "x-diesis-8" attr []

        Flat2 ->
            node "x-yfesis-2" attr []

        Flat4 ->
            node "x-yfesis-4" attr []

        Flat6 ->
            node "x-yfesis-6" attr []

        Flat8 ->
            node "x-yfesis-8" attr []


type Color
    = InheritColor
    | Red
