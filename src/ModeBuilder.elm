module ModeBuilder exposing (Model, Msg, init, update, view)

{-| Experimental modeling for constructing a mode.
-}

import Byzantine.Scale as Scale exposing (Scale)
import Components.RadioFieldset as RadioFieldset
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Extra exposing (viewIf)
import Maybe.Extra



-- MODEL


type alias Model =
    { classification : Maybe Classification
    , scale : Maybe Scale
    , strategy : Strategy
    }


init : Model
init =
    { classification = Nothing
    , scale = Nothing
    , strategy = SelectByScale
    }


type Strategy
    = SelectByScale
    | SelectByClassification
    | SelectBySignature


strategyToString : Strategy -> String
strategyToString strategy =
    case strategy of
        SelectByScale ->
            "By Scale"

        SelectByClassification ->
            "By Classification"

        SelectBySignature ->
            "By Signature"


strategyRadioConfig : RadioFieldset.Config Strategy Msg
strategyRadioConfig =
    RadioFieldset.baseConfig
        { itemToString = strategyToString
        , legendText = "Selection Strategy"
        , onSelect = SelectStrategy
        , options = [ SelectByScale, SelectByClassification, SelectBySignature ]
        }



-- MSG


type Msg
    = SelectScale (Maybe Scale)
    | SelectStrategy Strategy



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectScale maybeScale ->
            ( { model | scale = maybeScale }, Cmd.none )

        SelectStrategy strategy ->
            ( { model | strategy = strategy }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "p-4" ]
        [ RadioFieldset.view strategyRadioConfig model.strategy
        , viewIf (model.strategy == SelectByScale)
            (RadioFieldset.view scaleRadioConfig model.scale)
        ]


scaleRadioConfig : RadioFieldset.Config (Maybe Scale) Msg
scaleRadioConfig =
    RadioFieldset.baseConfig
        { itemToString = Maybe.Extra.unwrap "" Scale.name >> (++) "mode-builder-"
        , legendText = "Scale"
        , onSelect = SelectScale
        , options = List.map Just Scale.all
        }
        |> RadioFieldset.withCustomViewItem
            (Html.Extra.viewMaybe text << Maybe.map Scale.name)



-- CLASSIFICATION


{-| I think "Classification" (or maybe "OktoichosClassification") will work as a
term, at least for internal conceptual modeling. How this is exposed towards
users is a different question.

I'm wondering if it would be better to have just a flat list of eight rather than
a product type.

-}
type Classification
    = Classification Division Ordinal


type Division
    = Authentic
    | Plagal


type Ordinal
    = ModeOne
    | ModeTwo
    | ModeThree
    | ModeFour


toString : Classification -> String
toString (Classification division ordinal) =
    case ( division, ordinal ) of
        ( Authentic, ModeOne ) ->
            "Authentic Mode One"

        ( Authentic, ModeTwo ) ->
            "Authentic Mode Two"

        ( Authentic, ModeThree ) ->
            "Authentic Mode Three"

        ( Authentic, ModeFour ) ->
            "Authentic Mode Four"

        ( Plagal, ModeOne ) ->
            "Plagal Mode One"

        ( Plagal, ModeTwo ) ->
            "Plagal Mode Two"

        ( Plagal, ModeThree ) ->
            "Grave Mode"

        ( Plagal, ModeFour ) ->
            "Plagal Mode Four"



{-
   A Mode can be generally built from a classification plus a base.

   A base is a pitch (which, in all but one case, will be natural, but
   there does exist a grave variant on Ζω-flat, which prevents a simple
   definition on degree).



   But, not all pitches are actual bases.

   Bases:

   - Authentic 1: Κε, Πα
   - Authentic 2
   - Authentic 3: Γα
   - Authentic 4: Βου, Δι, Πα*
   - Plagal 1: Κε, Πα
   - Plagal 2
   - Plagal 3: Γα, Ζω, Ζω-flat
   - Plagal 4: Νη, Γα

-}
