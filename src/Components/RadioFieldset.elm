module Components.RadioFieldset exposing
    ( Config, baseConfig
    , Layout(..)
    , withCustomSelected, withCustomViewItem, withConditionalPostpend, withItemToString, withLayout
    , view
    )

{-| Builder for a radio button fieldset.


# Config Builder

@docs Config, baseConfig


# Layout

@docs Layout


# Config Options

@docs withCustomSelected, withCustomViewItem, withConditionalPostpend, withItemToString, withLayout


# View

@docs view

-}

import Html exposing (Html, div, fieldset, input, label, legend, text)
import Html.Attributes as Attr exposing (checked, class, type_)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy3)
import List.Extra
import String.Extra exposing (dasherize)
import Styles



-- LAYOUT


{-| Controls how radio button options are arranged inside the fieldset.

  - `Column` — the default; options stack vertically.
  - `FlexRow` — options are laid out in a wrapping horizontal row.
  - `TwoColumnGrid` — options fill the first column top-to-bottom, then the
    second column top-to-bottom, split as evenly as possible (the first column
    gets the extra item when the count is odd).

-}
type Layout
    = Column
    | FlexRow
    | TwoColumnGrid



-- CONFIG BUILDER


type Config a msg
    = Config
        { itemToString : a -> String
        , legendText : String
        , onSelect : a -> msg
        , options : List a
        , maybeBottomElement : Maybe (a -> Html msg)
        , optionIsSelected : Maybe (a -> a -> Bool)
        , viewItem : Maybe (a -> Html msg)
        , layout : Layout
        }


baseConfig :
    { itemToString : a -> String
    , legendText : String
    , onSelect : a -> msg
    , options : List a
    }
    -> Config a msg
baseConfig { itemToString, legendText, onSelect, options } =
    Config
        { itemToString = itemToString
        , legendText = legendText
        , onSelect = onSelect
        , options = options
        , maybeBottomElement = Nothing
        , optionIsSelected = Nothing
        , viewItem = Nothing
        , layout = Column
        }


{-| In case a basic equality check (`(==)`) won't work for determining if the
given radio button `a` option is the currently selected `a`, provide a custom
equivalency function. For example, this can be used to ignore the payload on a
custom type constructor.
-}
withCustomSelected : (a -> a -> Bool) -> Config a msg -> Config a msg
withCustomSelected optionIsSelected (Config config) =
    Config { config | optionIsSelected = Just optionIsSelected }


withCustomViewItem : (a -> Html msg) -> Config a msg -> Config a msg
withCustomViewItem viewItem (Config config) =
    Config { config | viewItem = Just viewItem }


{-| Add an element after the list of radio button input elements at the bottom
of the fieldset. The argument will be the selected option.
-}
withConditionalPostpend : (a -> Html msg) -> Config a msg -> Config a msg
withConditionalPostpend bottomElement (Config config) =
    Config { config | maybeBottomElement = Just bottomElement }


{-| Override the `itemToString` function used to derive element IDs and default
label text. Useful when two fieldsets share the same option type and would
otherwise produce identical `id` attributes.
-}
withItemToString : (a -> String) -> Config a msg -> Config a msg
withItemToString fn (Config config) =
    Config { config | itemToString = fn }


{-| Set the layout used to display the radio button options. Defaults to
`Column` if not specified. See [`Layout`](#Layout) for available options.
-}
withLayout : Layout -> Config a msg -> Config a msg
withLayout layout (Config config) =
    Config { config | layout = layout }



-- UNWRAP CONFIG ITEMS


getItemToStringFn : Config a msg -> a -> String
getItemToStringFn (Config { itemToString }) =
    itemToString


getLegendText : Config a msg -> String
getLegendText (Config { legendText }) =
    legendText


getOnSelectMsg : Config a msg -> a -> msg
getOnSelectMsg (Config { onSelect }) =
    onSelect


getOptions : Config a msg -> List a
getOptions (Config { options }) =
    options


getOptionIsSelected : Config a msg -> (a -> a -> Bool)
getOptionIsSelected (Config { optionIsSelected }) =
    Maybe.withDefault (==) optionIsSelected


getViewItem : Config a msg -> (a -> Html msg)
getViewItem (Config { itemToString, viewItem }) =
    Maybe.withDefault (text << itemToString) viewItem


getLayout : Config a msg -> Layout
getLayout (Config { layout }) =
    layout



-- VIEW


view : Config a msg -> a -> Html msg
view config selected =
    let
        renderedOptions =
            config
                |> getOptions
                |> List.map
                    (\option ->
                        lazy3 radioOption
                            config
                            (getOptionIsSelected config option selected)
                            option
                    )

        wrappedOptions =
            case getLayout config of
                Column ->
                    renderedOptions

                FlexRow ->
                    [ div [ Styles.flexRow, class "flex-wrap" ] renderedOptions ]

                TwoColumnGrid ->
                    let
                        ( col1, col2 ) =
                            List.Extra.splitAt
                                ((List.length renderedOptions + 1) // 2)
                                renderedOptions
                    in
                    [ div [ class "grid grid-cols-2" ]
                        (List.Extra.interweave col1 col2)
                    ]
    in
    wrappedOptions
        |> addBottomElement config selected
        -- we could include an optional instruction text here, too
        |> (::) (legend [ class "px-1" ] [ text (getLegendText config) ])
        |> fieldset [ Styles.borderRounded, class "px-2 pb-1 mb-2" ]


addBottomElement : Config a msg -> a -> List (Html msg) -> List (Html msg)
addBottomElement (Config { maybeBottomElement }) selected items =
    case maybeBottomElement of
        Just bottomElement ->
            List.Extra.push (bottomElement selected) items

        Nothing ->
            items


radioOption : Config a msg -> Bool -> a -> Html msg
radioOption config isSelected option =
    let
        id =
            "radio-option-" ++ dasherize (getItemToStringFn config option)
    in
    div [ Styles.flexRow, class "items-center" ]
        [ input
            [ type_ "radio"
            , Attr.name ("radio-" ++ dasherize (getLegendText config))
            , Attr.id id
            , class "cursor-pointer m-2"
            , checked isSelected
            , onClick (getOnSelectMsg config option)
            ]
            []
        , label
            [ Attr.for id, class "cursor-pointer" ]
            [ getViewItem config option ]
        ]
