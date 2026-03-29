module Components.Collapsible exposing
    ( Config, isOpen
    , withFirstChildTrigger, withTransition
    , TransitionDuration(..)
    , div, li
    )

{-| Builder for a CSS grid-based collapsible element.

    Collapsible.li (Collapsible.isOpen isOpen) [] [ triggerView, contentView ]

    Collapsible.li
        (Collapsible.isOpen isOpen |> Collapsible.withFirstChildTrigger)
        []
        [ triggerView, contentView ]


# Config Builder

@docs Config, isOpen
@docs withFirstChildTrigger, withTransition
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
    = ExternalTrigger
    | FirstChildTrigger


type TransitionDuration
    = TransitionQuick
    | TransitionStandard


{-| Base config. The trigger is expected to be a sibling outside this element.
-}
isOpen : Bool -> Config
isOpen open =
    Config
        { open = open
        , trigger = ExternalTrigger
        , transition = TransitionStandard
        }


{-| Use when the trigger and collapsible content are both direct children.
The collapsed content will need the transition class added to it.
-}
withFirstChildTrigger : Config -> Config
withFirstChildTrigger (Config config) =
    Config { config | trigger = FirstChildTrigger }


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
    classList
        [ ( "grid transition-[grid-template-rows] ease-in-out", True )
        , ( durationClass transition, True )
        , ( openClass trigger, open )
        , ( closedClass trigger, not open )
        ]


openClass : Trigger -> String
openClass trigger =
    case trigger of
        FirstChildTrigger ->
            "grid-rows-[auto_1fr]"

        ExternalTrigger ->
            "grid-rows-[1fr]"


closedClass : Trigger -> String
closedClass trigger =
    case trigger of
        FirstChildTrigger ->
            "grid-rows-[auto_0fr]"

        ExternalTrigger ->
            "grid-rows-[0fr]"


durationClass : TransitionDuration -> String
durationClass duration =
    case duration of
        TransitionQuick ->
            "duration-300"

        TransitionStandard ->
            "duration-800"
