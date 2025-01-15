module Martyria exposing (..)

import Html


martyria : Html.Html msg
martyria =
    Html.node "martyria" [] []


type Note
    = Foo


components : List String
components =
    [ "x-martyria-note-zo-low"
    , "x-martyria-note-ni-low"
    , "x-martyria-note-pa-low"
    , "x-martyria-note-vou-low"
    , "x-martyria-note-ga-low"
    , "x-martyria-note-di-low"
    , "x-martyria-note-ke-low"
    , "x-martyria-note-zo"
    , "x-martyria-note-ni"
    , "x-martyria-note-pa"
    , "x-martyria-note-vou"
    , "x-martyria-note-ga"
    , "x-martyria-note-di"
    , "x-martyria-note-ke"
    , "x-martyria-note-zo-high"
    , "x-martyria-note-ni-high"
    , "x-martyria-note-pa-high"
    , "x-martyria-note-vou-high"
    , "x-martyria-note-ga-high"
    , "x-martyria-note-di-high"
    , "x-martyria-note-ke-high"
    , "x-martyria-tick"
    , "x-martyria-zo-below"
    , "x-martyria-delta-below"
    , "x-martyria-alpha-below"
    , "x-martyria-legetos-below"
    , "x-martyria-nana-below"
    , "x-martyria-delta-dotted-below"
    , "x-martyria-alpha-dotted-below"
    , "x-martyria-hard-chromatic-pa-below"
    , "x-martyria-hard-chromatic-di-below"
    , "x-martyria-soft-chromatic-di-below"
    , "x-martyria-soft-chromatic-ke-below"
    , "x-martyria-zygos-below"
    , "x-martyria-zo-above"
    , "x-martyria-delta-above"
    , "x-martyria-alpha-above"
    , "x-martyria-legetos-above"
    , "x-martyria-nana-above"
    , "x-martyria-delta-dotted-above"
    , "x-martyria-alpha-dotted-above"
    , "x-martyria-hard-chromatic-pa-above"
    , "x-martyria-hard-chromatic-di-above"
    , "x-martyria-soft-chromatic-di-above"
    , "x-martyria-soft-chromatic-ke-above"
    , "x-martyria-zygos-above"
    ]