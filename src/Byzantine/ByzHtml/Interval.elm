module Byzantine.ByzHtml.Interval exposing (view)

import Byzantine.Accidental as Accidental
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

                Leap Oligon steps ->
                    note ("x-o" ++ ascendingStepsString steps) maybeAccidental

                Leap Petasti steps ->
                    note ("x-p" ++ ascendingStepsString steps) maybeAccidental

        Descending steps maybeAccidental ->
            case steps of
                DownOne ->
                    note "x-a" maybeAccidental

                DownTwo ->
                    note "x-e" maybeAccidental

                DownThree ->
                    note "x-ea" maybeAccidental

                downMore ->
                    note ("x-ch" ++ descendingStepsString downMore) maybeAccidental

        Ison ->
            note "x-i" Nothing


note : String -> Maybe Accidental.Accidental -> Html msg
note intervalNeume maybeAccidental =
    node "x-note"
        [ class "lg:text-2xl" ]
        [ node intervalNeume [] []
        , Html.Extra.viewMaybe (Accidental.view Accidental.Red) maybeAccidental
        ]
