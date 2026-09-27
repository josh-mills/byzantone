module Byzantine.ByzHtml.Interval exposing (view)

import Byzantine.Accidental exposing (Accidental)
import Byzantine.ByzHtml.Accidental as Accidental
import Byzantine.IntervalCharacter exposing (..)
import Html exposing (Html, node)
import Html.Attributes exposing (class)
import Html.Extra


{-| We're making this decently opinionated rather than atomic.
We may need to step back and make this simpler.
-}
view : IntervalCharacter -> Html msg
view interval =
    case interval of
        Ascending ascendingChar maybeAccidental ->
            case ascendingChar of
                SomaStep Oligon ->
                    note "x-o1" maybeAccidental

                SomaStep Petasti ->
                    note "x-p" maybeAccidental

                Kentimata ->
                    note "x-k" maybeAccidental

                Skip OligonKentimaBelow ->
                    note "x-o2" maybeAccidental

                Skip OligonKentimaRight ->
                    note "x-o2-m" maybeAccidental

                Skip PetastiOligon ->
                    note "x-p2" maybeAccidental

                Ypsili Oligon steps ->
                    note ("x-o" ++ ascendingStepsString steps) maybeAccidental

                Ypsili Petasti steps ->
                    note ("x-p" ++ ascendingStepsString steps) maybeAccidental

        Descending char maybeAccidental ->
            case char of
                Apostrophos ->
                    note "x-a" maybeAccidental

                Iporroi ->
                    note "x-y" maybeAccidental

                Elafron ->
                    note "x-e" maybeAccidental

                SynechesElafron ->
                    note "x-re" maybeAccidental

                ElafronApostrophos ->
                    note "x-ea" maybeAccidental

                Khamili descendingSteps ->
                    note ("x-ch" ++ descendingStepsString descendingSteps) maybeAccidental

        Ison ->
            note "x-i" Nothing


note : String -> Maybe Accidental -> Html msg
note intervalNeume maybeAccidental =
    node "x-note"
        [ class "lg:text-2xl" ]
        [ node intervalNeume [] []
        , Html.Extra.viewMaybe (Accidental.view Accidental.Red) maybeAccidental
        ]
