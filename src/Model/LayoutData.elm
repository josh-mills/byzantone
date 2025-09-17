module Model.LayoutData exposing
    ( LayoutData, init
    , LayoutSelection(..), Layout(..), layoutFor, layoutString
    , showSpacing
    )

{-| Layout data needed for proper rendering of the app, including both browser-
and user-controlled settings.


# LayoutData

@docs LayoutData, init


## Layout and LayoutSelection

@docs LayoutSelection, Layout, layoutFor, layoutString


# Dev Constants

@docs showSpacing

-}

import Browser.Dom as Dom


showSpacing : Bool
showSpacing =
    False


type alias LayoutData =
    { layoutSelection : LayoutSelection
    , pitchSpace : Dom.Element
    , viewport : Dom.Viewport
    }


init : { width : Float, height : Float } -> LayoutData
init viewportDimensions =
    { layoutSelection = Auto
    , pitchSpace = defaultElement
    , viewport = initViewport viewportDimensions
    }


initViewport : { width : Float, height : Float } -> Dom.Viewport
initViewport { width, height } =
    { scene = { width = 0, height = 0 }
    , viewport = { x = 0, y = 0, width = width, height = height }
    }


{-| Initial hardcoded element width of 64 prevents negative width settings which
enables a smooth css transition.
-}
defaultElement : Dom.Element
defaultElement =
    { scene = { width = 0, height = 0 }
    , viewport = { x = 0, y = 0, width = 0, height = 0 }
    , element = { x = 0, y = 0, width = 64, height = 0 }
    }


type LayoutSelection
    = Auto
    | Manual Layout


type Layout
    = Vertical
    | Horizontal


{-| For Auto layout, Vertical is the default. Only when the screen is relatively
short _and_ significantly wider than it is tall should the layout switch to
Horizontal.
-}
layoutFor : LayoutData -> Layout
layoutFor { layoutSelection, viewport } =
    case layoutSelection of
        Auto ->
            let
                ratio =
                    viewport.viewport.height / viewport.viewport.width
            in
            if viewport.viewport.height < 800 && ratio < 2.2 then
                Horizontal

            else
                Vertical

        Manual layout ->
            layout


layoutString : LayoutSelection -> String
layoutString layoutSelection =
    case layoutSelection of
        Auto ->
            "Auto"

        Manual Vertical ->
            "Vertical"

        Manual Horizontal ->
            "Horizontal"
