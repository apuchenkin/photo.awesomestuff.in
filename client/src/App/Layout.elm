module App.Layout exposing (..)

import Char
import String
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Router.Types exposing (Router, Action)

import App.Model exposing (State)
import App.Actions exposing (isLoading)
import App.Routes exposing (Route)
import Handler.Widgets exposing (..)
import Html.Keyed

fromCamelCase : String -> Maybe String
fromCamelCase = Maybe.map snd
  << String.uncons
  << String.concat
  << String.foldl (\c acc -> acc ++ [if Char.isUpper c then "-" ++ String.fromChar (Char.toLower c) else String.fromChar c]) []

isJust : Maybe a -> Bool
isJust a = case a of
  Just _ -> True
  Nothing -> False

keyedView : comparable -> Dict comparable a -> Maybe ( comparable, a )
keyedView k dict = Maybe.map (\v -> (k, v)) <| Dict.get k dict

layout : Router Route State -> State -> Dict String (Html (Action State)) -> Html (Action State)
layout router state views =
  let
    languageSelector' = languageSelector router
    defaultHeader = homeHeader router state.locale
    defaultFooter = footer router state.locale
    page = Maybe.andThen state.router.route (fromCamelCase << toString)
    header = Dict.get "header" views
  in
    Html.Keyed.node "div" [Attr.id "main", Attr.classList [(Maybe.withDefault "" page, isJust page)]] <| List.filterMap identity [
      Just <| ("loader", loader (isLoading state) state.transition.transitionIn)
    , keyedView "photo" views
    , Just <| ("content", Html.Keyed.node "div" [Attr.class "content"] <| List.filterMap identity [
            Just <| ("header", Maybe.withDefault defaultHeader header)
          , keyedView "navigation" views
          , keyedView "body" views
          , Just <| ("footer", Maybe.withDefault defaultFooter <| Dict.get "footer" views)
          ])
    , Just <| ("language-selector", languageSelector' state.router.route state.router.params state.locale)
    ]
