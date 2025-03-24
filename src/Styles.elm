module Styles exposing
    ( height, width
    , left, top
    , flexRow, flexRowCentered, flexCol
    , border, borderRounded, buttonClass, transition
    )

{-| Style attribute helpers and common Tailwind class combinations

TODO: shoud the positioning and size helpers take floats instead?


# Styles


## Sizing

@docs height, width


## Positioning

@docs left, top


# Tailwind Classes


## Layout

@docs flexRow, flexRowCentered, flexCol


## Misc

@docs border, borderRounded, buttonClass, transition

-}

import Html
import Html.Attributes exposing (class, style)



-- STYLE ATTRIBUTES: SIZING


{-| in px: `style="height: ${h}px;"`
-}
height : Int -> Html.Attribute msg
height h =
    style "height" (String.fromInt h ++ "px")


{-| in px: `style="width: ${w}px;"`
-}
width : Int -> Html.Attribute msg
width w =
    style "width" (String.fromInt w ++ "px")



-- STYLE ATTRIBUTES: POSITIONING


{-| in px: `style="left: ${l}px;"`
-}
left : Int -> Html.Attribute msg
left l =
    style "left" (String.fromInt l ++ "px")


{-| in px: `style="top: ${l}px;"`
-}
top : Int -> Html.Attribute msg
top l =
    style "top" (String.fromInt l ++ "px")



-- TAILWIND CLASSES: LAYOUT


{-| `class "flex flex-row"`

```css
.flex {
    display: flex;
}
.flex-row {
    flex-direction: row;
}
```

-}
flexRow : Html.Attribute msg
flexRow =
    class "flex flex-row"


{-| `class "flex flex-row justify-center"`

```css
.flex {
    display: flex;
}
.flex-row {
    flex-direction: row;
}
.justify-center {
    justify-content: center;
}
```

-}
flexRowCentered : Html.Attribute msg
flexRowCentered =
    class "flex flex-row justify-center"


{-| `class "flex flex-col"`

```css
.flex {
    display: flex;
}
.flex-col {
    flex-direction: column;
}
```

-}
flexCol : Html.Attribute msg
flexCol =
    class "flex flex-col"



-- TAILWIND CLASSES: MISC


{-| `class "border border-gray-300"`
-}
border : Html.Attribute msg
border =
    class "border border-gray-300"


{-| `class "border border-gray-300 rounded-md"`
-}
borderRounded : Html.Attribute msg
borderRounded =
    class "border border-gray-300 rounded-md"


{-| `class "bg-gray-200 my-2 py-1 px-3 rounded-md"`
-}
buttonClass : Html.Attribute msg
buttonClass =
    class "bg-gray-200 my-2 py-1 px-3 rounded-md"


{-| `class "transition-all duration-800"`
-}
transition : Html.Attribute msg
transition =
    class "transition-all duration-800"
