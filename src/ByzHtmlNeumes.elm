module ByzHtmlNeumes exposing (..)

{-| Wrapper for the Daniel Garthur's [byzhtml][1] package, which is a library of
custom web components for displaying Byzantine Chant neumes in HTML.

Because these are custom web components, the HTML document will require the
JavaScript and CSS. For right now, it seems like the easiest solution is just to
grab these from a CDN:

    <link
        rel="stylesheet"
        href="https://cdn.jsdelivr.net/gh/danielgarthur/byzhtml@1.0.16/dist/Neanes.css"
    />
    <script src="https://cdn.jsdelivr.net/gh/danielgarthur/byzhtml@1.0.16/dist/byzhtml.min.js"></script>

[1]: https://danielgarthur.github.io/byzhtml/


# Notes

This is a bit finicky, especially on Safari with rendering characters in
combination. The secret appears to be to wrap things in a `note` node, which
assists with letting the support web-component code parse the structure and know
when to combine symbols. It's curiously sensitive and looks towards whitespace
clues.

I wonder if the better way to go about this is to see if you can use the
underlying font more directly. See <https://danielgarthur.github.io/sbmufl/#/>
and <https://github.com/danielgarthur/sbmufl>.

-}

import Html exposing (Html, node)
import Html.Attributes exposing (class)


{-| The _Characters of (Intervallic) Quantity_ fall into two basic categories:
Σώματα and Πνεύματα. The σώματα can appear by themselves, but the πνεύματα will
only appear in combination with a σῶμα character.
-}
type Interval
    = Interval SomaCharacter (Maybe PneumaCharacter)


{-| There are six σώματα:

  - the Ἲσον (same note)
  - the Ὀλίγον (ascends 1 step)
  - the Ὀχεῖα (ascends 1 step)
  - the Πεταστή (ascends 1 step)
  - the Ἀπόστροφος (descends 1 step)
  - the Ὑποῥῤοή (descends 2 steps consecutively)
  - the Ἐλαφρόν (descends 2 steps)
  - the Χαμηλή (descends 4 steps)

-}
type SomaCharacter
    = Ison
    | Oligon
    | Oxeia
    | Petasti
    | Apostrophos
    | Elaphron
    | Chamili


{-| These should probably have some sort of placement indicator
that describes how exactly they're combined with the Soma character.

There are two of these:

  - the Κέντημα (ascends 2 steps)
  - the Κεντήματα (ascends 1 step)
  - the Ὑψηλή (ascends 4 stps)

-}
type PneumaCharacter
    = Kentima
    | Kentimata
    | Ipsili


type alias Note =
    { interval : Interval
    , lyric : Maybe Lyric
    }


type alias Lyric =
    String



-- MARTYRIA


type NoteName
    = Pa
    | Bou
    | Ga
    | Di
    | Ke
    | Zo
    | Ni


type alias Martyria msg =
    Html msg


martyria : String -> String -> Martyria msg
martyria top bottom =
    node "x-martyria"
        []
        [ node top [] []
        , node bottom [] []
        ]


martyria_pa_alpha : Martyria msg
martyria_pa_alpha =
    -- node "x-martyria"
    --     []
    --     [ node "x-martyria-note-pa" [] []
    --     , node "x-martyria-alpha-below" [] []
    --     ]
    martyria "x-martyria-note-pa" "x-martyria-alpha-below"


martyria_bou_legetos : Martyria msg
martyria_bou_legetos =
    martyria "x-martyria-note-vou" "x-martyria-legetos-below"


martyria_ga_nana : Martyria msg
martyria_ga_nana =
    martyria "x-martyria-note-ga" "x-martyria-nana-below"


martyria_di_delta : Martyria msg
martyria_di_delta =
    martyria "x-martyria-note-di" "x-martyria-delta-dotted-below"


martyria_ke_alpha : Martyria msg
martyria_ke_alpha =
    martyria "x-martyria-note-ke" "x-martyria-alpha-below"


martyria_zo_high_legetos : Martyria msg
martyria_zo_high_legetos =
    martyria "x-martyria-note-zo-high" "x-martyria-legetos-below"


martyria_ni_high_nana : Martyria msg
martyria_ni_high_nana =
    martyria "x-martyria-note-ni-high" "x-martyria-nana-below"


martyria_pa_high_alpha : Martyria msg
martyria_pa_high_alpha =
    martyria "x-martyria-note-pa-high" "x-martyria-alpha-below"



-- RENDERING


viewNote : Note -> Html msg
viewNote note =
    node "x-note"
        []
        [ case note.interval of
            _ ->
                Html.text "..."
        ]



-- EXPERIMENTS
-- note contents =
--     node "x-note" [] contents


ison =
    node "x-ison" [] []


psifiston =
    node "x-psifiston" [] []


lyric text =
    node "x-lyric" [] [ Html.text text ]



-- martyria a b =
--     node "x-martyria" [ class "text-xl text-red-900" ] [ a, b ]
-- testSign =
--     martyria
--         (node "x-martyria-note-pa" [ class "text-xl text-red-900" ] [])
--         (node "x-martyria-alpha-below" [ class "text-xl text-red-900" ] [])
