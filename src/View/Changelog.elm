module View.Changelog exposing (view)

import Date
import Html exposing (Html, div, h3, h4, li, span, text, ul)
import Html.Attributes exposing (class)
import Html.Extra
import Http
import Markdown
import Model.Changelog exposing (Changelog)
import Model.Changelog.Entry exposing (Entry)
import Model.Changelog.Version exposing (Version)
import RemoteData exposing (RemoteData)
import Update exposing (Msg)


view : RemoteData Http.Error Changelog -> Html Msg
view remoteChangelog =
    div [ class "p-4" ]
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


versionToString : Version -> String
versionToString { major, minor, patch } =
    "v"
        ++ String.fromInt major
        ++ "."
        ++ String.fromInt minor
        ++ "."
        ++ String.fromInt patch


formatVersionHeader : Entry -> Html msg
formatVersionHeader { version, date } =
    h3 [ class "text-lg font-semibold mb-2" ]
        [ span [ class "text-gray-900" ]
            [ text (versionToString version) ]
        , span [ class "text-gray-500" ]
            [ text (" (" ++ Date.format "MMMM d, y" date ++ ")") ]
        ]


viewEntry : Entry -> Html Msg
viewEntry entry =
    div [ class "border-l-4 border-l-gray-300 pl-4" ]
        [ formatVersionHeader entry
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
        Html.Extra.nothing

    else
        div [ class "space-y-1" ]
            [ h4 [ class "text-sm font-semibold uppercase tracking-wide text-gray-700" ]
                [ text sectionName ]
            , ul [ class "list-disc list-outside space-y-0.5 border-l-2 border-l-gray-300 pl-5" ]
                (List.map
                    (\change ->
                        li [ class "text-gray-700 text-sm [&_p]:m-0" ]
                            [ Markdown.toHtml
                                [ class "[&_a]:text-blue-600 [&_a]:underline [&_a:hover]:text-blue-800" ]
                                change
                            ]
                    )
                    changes
                )
            ]
