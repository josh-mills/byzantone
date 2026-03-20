module View.About exposing (view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Extra
import Http
import Markdown
import Remote.AboutCopy exposing (AboutCopy)
import RemoteData exposing (RemoteData)


view : RemoteData Http.Error AboutCopy -> Html msg
view remoteCopy =
    div []
        [ case remoteCopy of
            RemoteData.NotAsked ->
                Html.Extra.nothing

            RemoteData.Loading ->
                div [ class "text-center py-8 text-gray-600" ]
                    [ text "Loading..." ]

            RemoteData.Failure _ ->
                div [ class "bg-red-50 border border-red-200 rounded p-4 text-red-800" ]
                    [ text "Failed to load content." ]

            RemoteData.Success copy ->
                div [ class "[&_p]:mb-4 [&_a]:text-blue-700" ]
                    [ Markdown.toHtml [] copy.content ]
        ]
