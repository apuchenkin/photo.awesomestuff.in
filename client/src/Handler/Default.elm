module Handler.Default where

import Handler.Actions exposing (..)
import Lib.Router exposing (Router, Handler, DirtyState (..))
import Html exposing (div, text, Html, button)
import Effects  exposing (Effects, Never)
import Html.Attributes exposing (href)
import Handler.Routes as Route exposing (Route)
import Html.Events

homeHandler : Router State Route -> Handler State
homeHandler router =
  let
    home = Html.a [href "/"] [text "HOME"]
    view address state parsed = Just <| case parsed of
      Nothing   -> div [] [home, div [] <| List.map (\c -> Html.a ((router.bindForward (Route.Category c.name)) []) [text c.title]) state]
      Just html -> div [] [home, html]
  in
    {
      view = view,
      inputs = [
        loadCategories
      ]
    }

categoryHandler : Router State Route -> String -> Handler State
categoryHandler router category =
  let
    _ = Debug.log "category:" category
    view address state parsed = Just <| div [] [text <| category]
  in
    {
      view = view,
      inputs = [
        (\state -> case List.filter (\c -> c.name == category) state of
          []         -> router.forward Route.Error state
          [category] -> DirtyState (state, Effects.none)
          _          -> router.forward Route.Error state
        )
      ]
    }

-- adminHandler : Handler State
-- adminHandler =
--   let
--     view address state parsed = Just <| div [] [Html.button [Html.Events.onClick address (updateCategories [])] [text "admin"]]
--   in
--     {
--       view = view,
--       inputs = []
--     }

errorHandler : Router State Route -> Handler State
errorHandler _ =
  let
    view address state parsed = Just <| div [] [text <| "404"]
  in
    {
      view = view,
      inputs = [
        -- forward
      ]
    }

forwardHandler : Router State Route -> Handler State
forwardHandler router =
  let
    view address state parsed = Nothing
  in
    {
      view = view,
      inputs = [
        router.forward Route.Error
      ]
    }
