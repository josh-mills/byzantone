module View.ModeData exposing (view)

import Byzantine.Accidental as Accidental
import Byzantine.Degree as Degree exposing (Degree)
import Byzantine.Mode as Mode exposing (Mode)
import Byzantine.Scale as Scale
import Html exposing (Html, button, div, h3, li, span, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List
import Model exposing (Modal(..))
import Styles
import Update exposing (Msg(..))


view : Mode -> Html Msg
view mode =
    let
        modeData =
            Mode.data mode
    in
    div []
        [ h3 [ class "font-heading text-xl" ]
            [ text (Mode.toString mode) ]
        , ul [ class "list-disc ps-4 my-2" ]
            [ listItem "Scale"
                (text (Scale.name modeData.scale))
            , listItem "Base"
                (Degree.text modeData.dominantTones.base)
            , listItem "Ison"
                (degreeList modeData.isonOptions)
            , listItem "Cadence Points"
                (ul [ class "list-disc ps-6" ]
                    [ listItemLevel2 "Final"
                        (Degree.text modeData.dominantTones.cadencePoints.final)
                    , listItemLevel2 "Complete"
                        (degreeList modeData.dominantTones.cadencePoints.complete)
                    , listItemLevel2 "Medial"
                        (degreeList modeData.dominantTones.cadencePoints.medial)
                    , listItemLevel2 "Incomplete"
                        (degreeList modeData.dominantTones.cadencePoints.incomplete)
                    ]
                )
            , listItem "Available Melodic Attractions"
                (ul [ class "list-disc ps-6" ]
                    (List.map attraction modeData.possibleInflections)
                )
            ]
        , button
            [ Styles.buttonClass
            , class "my-2"
            , onClick (SelectModal (ModeModal Nothing))
            ]
            [ text "Back" ]
        ]


{-| we'll probably need to think about styling here
-}
listItem : String -> Html msg -> Html msg
listItem label content =
    li []
        [ span [ class "font-heading text-lg" ] [ text label, text ": " ]
        , content
        ]


{-| we'll probably need to think about styling here
-}
listItemLevel2 : String -> Html msg -> Html msg
listItemLevel2 label content =
    li []
        [ span [ class "font-heading" ] [ text label, text ": " ]
        , content
        ]


attraction : Mode.Inflection -> Html msg
attraction inflection =
    li []
        [ Degree.textOctave inflection.degree
        , text ": "
        , inflection.accidentals
            |> List.map (Accidental.toString >> text)
            |> List.intersperse (text ", ")
            |> span []
        ]


degreeList : List Degree -> Html msg
degreeList degrees =
    span []
        (List.map Degree.text degrees
            |> List.intersperse (text ", ")
        )
