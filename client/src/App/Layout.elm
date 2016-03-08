module App.Layout where

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Router.Types exposing (Router)

import App.Actions exposing (State, isLoading)
import App.Routes exposing (Route)
import Handler.Widgets exposing (..)

layout : Router Route State -> State -> Dict String Html -> Html
layout router state views =
  let
    defaultHeader = Html.header [] [Html.text "Default header"]
    defaultFooter = Html.footer [] [Html.text "Default footer"]
    defaultBody = Html.div [Attr.class "body"] []
  in Html.div [Attr.id "main"] [
    loader (isLoading state)
  , Maybe.withDefault (Html.div [] []) <| Dict.get "photo" views
  , Html.div [Attr.class "content"] [
        Maybe.withDefault defaultHeader <| Dict.get "header" views
      , Maybe.withDefault (Html.div [] []) <| Dict.get "navigation" views
      , Maybe.withDefault defaultBody   <| Dict.get "body" views
      , Maybe.withDefault defaultFooter <| Dict.get "footer" views
      , languageSelector router state
      ]
  ]
