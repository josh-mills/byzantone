module RadioFieldset exposing (Config, view)

import Html exposing (Html, div, fieldset, input, label, legend, text)
import Html.Attributes as Attr exposing (checked, class, type_)
import Html.Events exposing (onClick)
import Html.Lazy
import String.Extra exposing (dasherize)
import Styles


type alias Config a msg =
    { itemToString : a -> String
    , legendText : String
    , onSelect : a -> msg
    , options : List a
    , selected : a
    , viewItem : Maybe (a -> Html msg)
    }


view : Config a msg -> Html msg
view config =
    Html.Lazy.lazy
        (\config_ ->
            List.map (radioOption config_) config_.options
                |> (::) (legend [ class "px-1" ] [ text config_.legendText ])
                |> fieldset [ Styles.borderRounded, class "px-2 pb-1 mb-2" ]
        )
        config


radioOption : Config a msg -> a -> Html msg
radioOption config option =
    let
        itemName =
            config.itemToString option

        id =
            "radio-option-" ++ dasherize itemName
    in
    div [ Styles.flexRow, class "items-center" ]
        [ input
            [ type_ "radio"
            , Attr.name ("radio-" ++ dasherize config.legendText)
            , Attr.id id
            , class "cursor-pointer m-2"
            , checked (config.selected == option)
            , onClick (config.onSelect option)
            ]
            []
        , label
            [ Attr.for id, class "cursor-pointer" ]
            [ case config.viewItem of
                Just viewItem ->
                    viewItem option

                Nothing ->
                    text (config.itemToString option)
            ]
        ]
