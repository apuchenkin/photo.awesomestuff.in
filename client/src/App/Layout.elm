module App.Layout where

import Char
import String
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Router.Types exposing (Router)

import App.Model exposing (State)
import App.Actions exposing (isLoading)
import App.Routes exposing (Route)
import Handler.Widgets exposing (..)

fromCamelCase : String -> Maybe String
fromCamelCase = Maybe.map snd
  << String.uncons
  << String.concat
  << String.foldl (\c acc -> acc ++ [if Char.isUpper c then "-" ++ String.fromChar (Char.toLower c) else String.fromChar c]) []

isJust : Maybe a -> Bool
isJust a = case a of
  Just _ -> True
  Nothing -> False

layout : Router Route State -> State -> Dict String Html -> Html
layout router state views =
  let
    defaultHeader = homeHeader router state.locale
    defaultFooter = footer router state.locale
    defaultBody = Html.div [Attr.class "body"] []
    page = Maybe.andThen state.router.route (fromCamelCase << toString)
  in Html.div [Attr.id "main", Attr.classList [(Maybe.withDefault "" page, isJust page)]] [
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
