module Components.Collapsible exposing
    ( Config, isOpen
    , withExternalTrigger, withTransition
    , TransitionDuration(..)
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
@docs withExternalTrigger, withTransition
@docs TransitionDuration


# View

@docs div, li

-}

import Html exposing (Html)
import Html.Attributes exposing (classList)



-- CONFIG BUILDER


type Config
    = Config
        { open : Bool
        , trigger : Trigger
        , transition : TransitionDuration
        }


type Trigger
    = TriggerChild
    | TriggerSibling


type TransitionDuration
    = TransitionQuick
    | TransitionStandard


{-| Base config. Expects a trigger element and collapsible content as direct
children.
-}
isOpen : Bool -> Config
isOpen open =
    Config
        { open = open
        , trigger = TriggerChild
        , transition = TransitionStandard
        }


{-| Use when the trigger is a sibling outside this element, not a child.
-}
withExternalTrigger : Config -> Config
withExternalTrigger (Config config) =
    Config { config | trigger = TriggerSibling }


{-| Override the transition duration. Defaults to `TransitionStandard`.
-}
withTransition : TransitionDuration -> Config -> Config
withTransition duration (Config config) =
    Config { config | transition = duration }



-- VIEW


{-| Render as a `<div>`.
-}
div : Config -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
div config attrs contents =
    Html.div (gridAttr config :: attrs) contents


{-| Render as a `<li>`.
-}
li : Config -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
li config attrs contents =
    Html.li (gridAttr config :: attrs) contents



-- INTERNAL


gridAttr : Config -> Html.Attribute msg
gridAttr (Config { open, trigger, transition }) =
    let
        ( openClass, closedClass ) =
            case trigger of
                TriggerChild ->
                    ( "grid-rows-[auto_1fr]", "grid-rows-[auto_0fr]" )

                TriggerSibling ->
                    ( "grid-rows-[1fr]", "grid-rows-[0fr]" )
    in
    classList
        [ ( "grid transition-[grid-template-rows] ease-in-out", True )
        , ( durationClass transition, True )
        , ( openClass, open )
        , ( closedClass, not open )
        ]


durationClass : TransitionDuration -> String
durationClass duration =
    case duration of
        TransitionQuick ->
            "duration-300"

        TransitionStandard ->
            "duration-800"
