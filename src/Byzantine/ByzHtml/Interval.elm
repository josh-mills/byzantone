module Byzantine.ByzHtml.Interval exposing (view)

import Byzantine.IntervalCharacter exposing (..)
import Html exposing (Html, node)


view : IntervalCharacter -> Html msg
view interval =
    case interval of
        Ascending ascendingChar ->
            case ascendingChar of
                SomaStep Oligon ->
                    node "x-o1" [] []

                SomaStep Petasti ->
                    node "x-p" [] []

                Kentimata ->
                    node "x-k" [] []

                Skip OligonKentimaBelow ->
                    node "x-o2" [] []

                Skip OligonKentimaRight ->
                    node "x-o2-m" [] []

                Skip PetastiOligon ->
                    node "x-p2" [] []

                Leap Oligon steps ->
                    node ("x-o" ++ ascendingStepsString steps) [] []

                Leap Petasti steps ->
                    node ("x-p" ++ ascendingStepsString steps) [] []

        Descending steps ->
            case steps of
                DownOne ->
                    node "x-a" [] []

                DownTwo ->
                    node "x-e" [] []

                DownThree ->
                    node "x-ea" [] []

                downMore ->
                    node ("x-ch" ++ descendingStepsString downMore) [] []

        Ison ->
            node "x-i" [] []
