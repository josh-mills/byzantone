module Byzantine.ByzHtml.Martyria exposing (view, viewWithAttributes)

import Byzantine.Degree exposing (Degree(..))
import Byzantine.Martyria as Martyria exposing (Martyria, ModalSignature(..))
import Html exposing (Html)


view : Martyria -> Html msg
view martyria =
    viewWithAttributes [] martyria


viewWithAttributes : List (Html.Attribute msg) -> Martyria -> Html msg
viewWithAttributes attributes martyria =
    let
        ( degree, signature ) =
            Martyria.unwrap martyria
    in
    if positionDegreeBelow degree then
        Html.node "x-martyria" attributes [ viewSignature False signature, viewDegree degree ]

    else
        Html.node "x-martyria" attributes [ viewDegree degree, viewSignature True signature ]


positionDegreeBelow : Degree -> Bool
positionDegreeBelow degree =
    case degree of
        GA ->
            True

        DI ->
            True

        KE ->
            True

        _ ->
            False


viewDegree : Degree -> Html msg
viewDegree degree =
    let
        node string =
            Html.node string [] []
    in
    case degree of
        GA ->
            node "x-martyria-note-ga-low"

        DI ->
            node "x-martyria-note-di-low"

        KE ->
            node "x-martyria-note-ke-low"

        Zo ->
            node "x-martyria-note-zo"

        Ni ->
            node "x-martyria-note-ni"

        Pa ->
            node "x-martyria-note-pa"

        Bou ->
            node "x-martyria-note-vou"

        Ga ->
            node "x-martyria-note-ga"

        Di ->
            node "x-martyria-note-di"

        Ke ->
            node "x-martyria-note-ke"

        Zo_ ->
            node "x-martyria-note-zo-high"

        Ni_ ->
            node "x-martyria-note-ni-high"

        Pa_ ->
            node "x-martyria-note-pa-high"

        Bou_ ->
            node "x-martyria-note-vou-high"

        Ga_ ->
            node "x-martyria-note-ga-high"


{-|


## Notes on Names

  - Baris in the SBMuFL terminology is indicated as `martyriaZoBelow`

  - The chromatic signature that appears with Pa in the hard chromatic genus and
    Di in the soft chromatic is derived from a highly stylized βύτα (used as
    ordinal for 2, indicating the second mode), which looked something like a
    modern "u". For the soft chromatic, it includes the two dots. SBMuFL calls
    these "hardChromaticPa" and "softChromaticDi", respectively.

  - The chromatic signature that appears with Di in the hard chromatic genus and
    Pa in the soft is a small circle with a left-to-right upward diagonal slash
    called _nenano_. This is altered with a crossbar on the upper diagonal in
    the soft chromatic. SBMuFL calls these "hardChromaticDi" and
    "softChromaticKe".

-}
viewSignature : Bool -> ModalSignature -> Html msg
viewSignature positionBelow signature =
    let
        position =
            if positionBelow then
                "-below"

            else
                "-above"

        node string =
            Html.node (string ++ position) [] []
    in
    case signature of
        Baris ->
            node "x-martyria-zo"

        Delta ->
            node "x-martyria-delta"

        Alpha ->
            node "x-martyria-alpha"

        Legetos ->
            node "x-martyria-legetos"

        NaNa ->
            node "x-martyria-nana"

        DeltaDotted ->
            node "x-martyria-delta-dotted"

        AlphaDotted ->
            node "x-martyria-alpha-dotted"

        BetaHard ->
            node "x-martyria-hard-chromatic-pa"

        NenanoHard ->
            node "x-martyria-hard-chromatic-di"

        BetaSoft ->
            node "x-martyria-soft-chromatic-di"

        NenanoSoft ->
            node "x-martyria-soft-chromatic-ke"
