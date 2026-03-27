module Components.Collapsible exposing
    ( Config, isOpen
    , withExternalTrigger
    , div, li
    )

{-| Builder for a CSS grid-based collapsible element.

    Collapsible.li (Collapsible.isOpen isOpen) [] [ triggerView, contentView ]

    Collapsible.div
        (Collapsible.isOpen isOpen |> Collapsible.withExternalTrigger)
        [ class "flex-1 mx-4" ]
        [ contentView ]


# Config Builder

@docs Config, isOpen
@docs withExternalTrigger


# View

@docs div, li

-}

import Html exposing (Html)
import Html.Attributes exposing (classList)



-- CONFIG BUILDER


type Config
    = Config
        { open : Bool
        , triggerIsChild : Bool
        }


{-| Base config. Expects a trigger element and collapsible content as direct
children.
-}
isOpen : Bool -> Config
isOpen open =
    Config
        { open = open
        , triggerIsChild = True
        }


{-| Use when the trigger is a sibling outside this element, not a child.
-}
withExternalTrigger : Config -> Config
withExternalTrigger (Config c) =
    Config { c | triggerIsChild = False }



-- VIEW


{-| Render as a `<div>`.
-}
div : Config -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
div c attrs contents =
    Html.div (gridAttr c :: attrs) contents


{-| Render as a `<li>`.
-}
li : Config -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
li c attrs contents =
    Html.li (gridAttr c :: attrs) contents



-- INTERNAL


gridAttr : Config -> Html.Attribute msg
gridAttr (Config { open, triggerIsChild }) =
    if triggerIsChild then
        classList
            [ ( "grid transition-[grid-template-rows] duration-300 ease-in-out", True )
            , ( "grid-rows-[auto_1fr]", open )
            , ( "grid-rows-[auto_0fr]", not open )
            ]

    else
        classList
            [ ( "grid transition-[grid-template-rows] duration-300 ease-in-out", True )
            , ( "grid-rows-[1fr]", open )
            , ( "grid-rows-[0fr]", not open )
            ]
