module Handler.Layout where

import Dict exposing (Dict)
import Html exposing (Html)
import Handler.Widgets exposing (..)
import Handler.Actions exposing (State)
import Handler.Routes exposing (Route)
import Router.Types exposing (Router)

layout : Router Route State -> State -> Dict String Html -> Html
layout router state parsed =
  let
    defaultHeader = Html.header [] [Html.text "Default header"]
    defaultFooter = Html.footer [] [Html.text "Default footer"]
    defaultBody = Html.div [] [Html.text "empty"]
  in Html.div [] [
    loader state.isLoading
  , languageSelector router state
  , Maybe.withDefault defaultHeader <| Dict.get "header" parsed
  , Maybe.withDefault defaultBody   <| Dict.get "body" parsed
  , Maybe.withDefault defaultFooter <| Dict.get "footer" parsed
  ]
