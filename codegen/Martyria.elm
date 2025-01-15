module Martyria exposing (martyriaFile)

{-| Module for generating Martyria.

The odd conceptual thing about this is that these are essentially an emergent,
presentational concept.


# Notes

Placement rules of martyria seem to be that the note is in the upper position
and the signature in the lower, except for the lowest register (`KE`, `DI`, and
`GA`), which are reversed.

-}

import Elm
import Elm.Annotation as Type
import Gen.CodeGen.Generate as Generate
import Gen.Html as Html


martyriaFile : List String -> Elm.File
martyriaFile components =
    let
        filtered =
            List.filter (String.startsWith "x-martyria") components

        ( notes, rest ) =
            List.partition (String.contains "note") filtered
    in
    Elm.file [ "Martyria" ]
        [ martyriaDeclaration
        , Elm.customType "Note"
            [ Elm.variant "Foo" ]
        , Elm.declaration "components"
            (Elm.list (List.map Elm.string filtered))
        ]


{-| Wraps martyria neumes together. This will need to be a `node` that takes two
neumes.
-}
martyriaDeclaration : Elm.Declaration
martyriaDeclaration =
    Elm.declaration "martyria" (Html.node "martyria" [] [])
