module View.ModeData exposing (view)

import Byzantine.Degree as Degree
import Byzantine.Mode as Mode exposing (Mode)
import Byzantine.Scale as Scale
import Html exposing (Html, div, h3, text)
import Html.Attributes exposing (class)


view : Mode -> Html msg
view mode =
    let
        modeData =
            Mode.data mode
    in
    div []
        [ h3 [ class "font-heading text-xl" ]
            [ text (Mode.toString mode) ]
        , div []
            [ text "Scale: "
            , text (Scale.name modeData.scale)
            ]
        , div []
            [ text "Base: "
            , Degree.text modeData.dominantTones.base
            ]
        , div []
            [ text "Final: "
            , Degree.text modeData.dominantTones.cadencePoints.final
            ]
        ]
