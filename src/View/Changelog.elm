module View.Changelog exposing (view)

import Html exposing (Html, button, div, h3, h4, li, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Model.Changelog exposing (Changelog)
import RemoteData exposing (RemoteData)
import Update exposing (Msg(..))


view : RemoteData Http.Error Changelog -> Html Msg
view remoteChangelog =
    div [ class "p-4 max-w-4xl" ]
        [ case remoteChangelog of
            RemoteData.NotAsked ->
                div [ class "text-center py-8 text-gray-600" ]
                    [ text "Loading changelog..." ]

            RemoteData.Loading ->
                div [ class "text-center py-8 text-gray-600" ]
                    [ text "Loading changelog..." ]

            RemoteData.Failure error ->
                div [ class "bg-red-50 border border-red-200 rounded p-4 text-red-800" ]
                    [ text "Failed to load changelog: "
                    , text (httpErrorToString error)
                    , div [ class "mt-2" ]
                        [ button
                            [ class "bg-red-600 hover:bg-red-700 text-white px-3 py-1 rounded text-sm"
                            , onClick RefreshChangelog
                            ]
                            [ text "Retry" ]
                        ]
                    ]

            RemoteData.Success changelog ->
                div [ class "space-y-4" ]
                    (List.map viewEntry changelog.entries)
        ]


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody body ->
            "Bad response body: " ++ body


formatVersionHeader : String -> String -> String
formatVersionHeader version date =
    "v" ++ version ++ " (" ++ date ++ ")"


viewEntry : Model.Changelog.Entry -> Html Msg
viewEntry entry =
    div [ class "border-l-4 border-l-gray-300 pl-4" ]
        [ h3 [ class "text-lg font-semibold mb-2 text-gray-900" ]
            [ text (formatVersionHeader entry.version entry.date) ]
        , div [ class "space-y-2" ]
            [ viewChangeSection "Added" entry.changes.added
            , viewChangeSection "Changed" entry.changes.changed
            , viewChangeSection "Fixed" entry.changes.fixed
            , viewChangeSection "Deprecated" entry.changes.deprecated
            , viewChangeSection "Removed" entry.changes.removed
            , viewChangeSection "Security" entry.changes.security
            ]
        ]


viewChangeSection : String -> List String -> Html Msg
viewChangeSection sectionName changes =
    if List.isEmpty changes then
        text ""

    else
        div [ class "space-y-1" ]
            [ h4 [ class "text-sm font-semibold uppercase tracking-wide text-gray-700" ]
                [ text sectionName ]
            , ul [ class "list-disc list-inside space-y-0.5 border-l-2 border-l-gray-300 pl-3" ]
                (List.map (\change -> li [ class "text-gray-700" ] [ text change ]) changes)
            ]
