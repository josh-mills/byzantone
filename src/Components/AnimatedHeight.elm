module Components.AnimatedHeight exposing (view, viewWithDuration)

{-| Wrapper for the `<animated-height>` custom element, which smoothly
animates its height when its content changes.

The element clips its children with `overflow: hidden` and animates its
own height whenever its direct child's height changes. Pass children via
a keyed node to also trigger the slide transition:

    AnimatedHeight.view
        (Html.Keyed.node "div" [] [ ( key, content ) ])

@docs view, viewWithDuration

-}

import Html exposing (Html)
import Html.Attributes


{-| Render an animated-height element with the standard 300ms transition.
-}
view : Html msg -> Html msg
view =
    viewWithDuration 300


{-| Render an animated-height element with a custom duration in milliseconds.
Pass 0 to disable the animation entirely.
-}
viewWithDuration : Int -> Html msg -> Html msg
viewWithDuration duration content =
    Html.node "animated-height"
        [ Html.Attributes.attribute "duration" (String.fromInt duration) ]
        [ content ]
