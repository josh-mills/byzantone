module Copy exposing (about)

import Html exposing (Html, div, p, text)
import Html.Attributes as Attr exposing (class)
import Html.Lazy exposing (lazy, lazy2)



-- ABOUT


about : Html msg
about =
    lazy
        (\_ ->
            div [ class "flex flex-col gap-4" ]
                [ origins
                , aboutTheAuthor
                ]
        )
        ()


origins : Html msg
origins =
    p []
        [ text """
ByzanTone is an ear training tool to assist in learning the pitches and interval of Byzantine chant.
This project had its origins in the author's own struggles to learn Byzantine chant as a musician trained in the western classical tradition.
The pitches and intervals of Byzantine chant cannot be faithfully reproduced on the piano, nor can they be accurately notated with standard staff notation.
This tool provides an accessible way to hear the exact tuning of the pitches and visualize their relationships as an aid to practicing the psaltic art.
    """
        ]


aboutTheAuthor : Html msg
aboutTheAuthor =
    p []
        [ text """ByzanTone is written and maintained by Josh Mills, a software engineer, a conservatory-trained composer and music theorist, and a Greek Orthodox chanter living in western Massachusetts. """
        , text "This is an open-source project; code is available at "
        , externalLink "https://github.com/josh-mills/byzantone" "GitHub"
        , text "."
        ]


externalLink : String -> String -> Html msg
externalLink href linkText =
    lazy2
        (\href_ text_ ->
            Html.a
                [ Attr.href href_
                , Attr.target "_blank"
                , class "text-blue-700"
                ]
                [ text text_ ]
        )
        href
        linkText
