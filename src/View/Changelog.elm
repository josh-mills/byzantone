module View.Changelog exposing (view)

import Html exposing (Html, button, div, h3, h4, li, span, text, ul)
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
                div [ class "space-y-8" ]
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


viewEntry : Model.Changelog.Entry -> Html Msg
viewEntry entry =
    div [ class "border-l-4 border-gray-200 pl-4 first:border-l-green-500" ]
        [ h3 [ class "text-xl font-semibold mb-3" ]
            [ text entry.version
            , case entry.date of
                Just date ->
                    span [ class "text-gray-500 font-normal ml-2" ] [ text ("– " ++ date) ]

                Nothing ->
                    text ""
            ]
        , div [ class "space-y-4" ]
            [ viewChangeSection "Added" "text-green-700 border-l-green-500" entry.changes.added
            , viewChangeSection "Changed" "text-blue-700 border-l-blue-500" entry.changes.changed
            , viewChangeSection "Fixed" "text-red-700 border-l-red-500" entry.changes.fixed
            , viewChangeSection "Deprecated" "text-orange-700 border-l-orange-500" entry.changes.deprecated
            , viewChangeSection "Removed" "text-gray-700 border-l-gray-500" entry.changes.removed
            , viewChangeSection "Security" "text-purple-700 border-l-purple-500" entry.changes.security
            ]
        ]


viewChangeSection : String -> String -> List String -> Html Msg
viewChangeSection sectionName colorClasses changes =
    if List.isEmpty changes then
        text ""

    else
        div [ class "space-y-2" ]
            [ h4 [ class ("text-sm font-semibold uppercase tracking-wide " ++ colorClasses) ]
                [ text sectionName ]
            , ul [ class ("list-disc list-inside space-y-1 border-l-2 pl-3 " ++ colorClasses) ]
                (List.map (\change -> li [ class "text-gray-700" ] [ text change ]) changes)
            ]
