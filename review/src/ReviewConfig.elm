module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import NoMissingTypeAnnotation
import NoPrematureLetComputation
import NoRedundantlyQualifiedType
import NoSimpleLetBody
import NoUnnecessaryTrailingUnderscore
import NoUnused.Dependencies
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    [ NoMissingTypeAnnotation.rule
    , NoPrematureLetComputation.rule
    , NoRedundantlyQualifiedType.rule |> ignoreForGenerated
    , NoSimpleLetBody.rule
    , NoUnnecessaryTrailingUnderscore.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Variables.rule
    ]


ignoreForGenerated : Rule -> Rule
ignoreForGenerated =
    Rule.ignoreErrorsForDirectories [ "generated/" ]
