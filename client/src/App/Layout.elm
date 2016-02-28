module App.Layout where

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Router.Types exposing (Router)

import App.Actions exposing (State)
import App.Routes exposing (Route)
import Handler.Widgets exposing (..)

layout : Router Route State -> State -> Dict String Html -> Html
layout router state views =
  let
    defaultHeader = Html.header [] [Html.text "Default header"]
    defaultFooter = Html.footer [] [Html.text "Default footer"]
    defaultBody = Html.div [Attr.class "body"] []
  in Html.div [Attr.class "main"] <| List.filterMap identity [
     Just <| loader state.isLoading
  ,  Dict.get "photo" views
  ,  Just <| Html.div [Attr.class "content"] <| List.filterMap identity [
        Just <| Maybe.withDefault defaultHeader <| Dict.get "header" views
      , Dict.get "navigation" views
      , Just <| Maybe.withDefault defaultBody   <| Dict.get "body" views
      , Just <| Maybe.withDefault defaultFooter <| Dict.get "footer" views
      , Just <| languageSelector router state
      ]
  ]
