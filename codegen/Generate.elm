module Generate exposing (main)

{-| <https://danielgarthur.github.io/byzhtml/#/>
-}

import Elm
import Elm.Annotation as Type
import Gen.CodeGen.Generate as Generate
import Gen.Html as Html
import Martyria


main : Program String () ()
main =
    Generate.fromText
        (\inputText ->
            let
                components =
                    String.split "\n" inputText
                        |> List.filterMap parseComponent
            in
            [ Martyria.martyriaFile components ]
        )


{-| Grabs the string in the Components column of the markdown file, e.g.,
"x-oligon-kentima-above".

When we convert this into an Elm helper, we should camel-case the name and drop
the leading `"x-"`, but that needs to be in the resulting node for the byzhtml
library to pick it up.

-}
parseComponent : String -> Maybe String
parseComponent string =
    if String.startsWith "| x" string then
        string
            |> String.split "|"
            |> List.filter (not << String.isEmpty)
            |> List.head
            |> Maybe.map String.trim

    else
        Nothing


{- _ | Other things we'll need:

  - x-martyria - Wraps martyria neumes together

  - x-neume - Displays the neume specified by the `name` attribute.

  - x-note - Wraps a quantitative neume together with its supporting characters
    and lyrics.

These are available in the `component-list-other.md` file. Nto sure if it's
worth running a code-generation process on these, or just doing them manually.
It's only a handful.

-}



-- martyriaFile : List String -> Elm.File
-- martyriaFile components =
--     let
--         filtered =
--             List.filter (String.startsWith "x-martyria") components
--         ( notes, rest ) =
--             List.partition (String.contains "note") filtered
--     in
--     Elm.file [ "Martyria" ]
--         [ martyriaDeclaration
--         , Elm.customType "Note"
--             [ Elm.variant "Foo" ]
--         , Elm.declaration "components"
--             (Elm.list (List.map Elm.string filtered))
--         ]


{-| Wraps martyria neumes together. This will need to be a `node` that takes two
neumes.
-}
martyriaDeclaration : Elm.Declaration
martyriaDeclaration =
    Elm.declaration "martyria" (Html.node "martyria" [] [])
