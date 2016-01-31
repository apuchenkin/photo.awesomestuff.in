module Handler.Default where

import Handler.Actions exposing (..)
import Lib.Router exposing (Router (..), Handler, Response (..))
import Html exposing (div, text, Html, button)
import Effects  exposing (Effects, Never)
import Html.Attributes exposing (href)
import Handler.Routes as Route exposing (Route)
import Html.Events

homeHandler : Router Route State -> Handler State
homeHandler (Router router) =
  let
    home = Html.a [href "/"] [text "HOME"]
    view address state parsed = Just <| case parsed of
      Nothing   -> div [] [home, div [] <| List.map (\c -> Html.a ((router.bindForward (Route.Category c.name)) []) [text c.title]) state.categories]
      Just html -> div [] [home, html]
  in
    {
      view = view,
      inputs = [
        loadCategories
      ]
    }

categoryHandler : Router Route State -> String -> Handler State
categoryHandler (Router router) category =
  let
    view address state parsed = let _ = Debug.log "category" state in Just <| div [] [text <| category]
  in
    {
      view = view,
      inputs = [
        (\state -> let _ = Debug.log "categoryi" state in case List.filter (\c -> c.name == category) state.categories of
          []         -> router.forward Route.Error state
          [category] -> Response (state, Effects.none)
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

errorHandler : Router Route State -> Handler State
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

forwardHandler : Router Route State -> Handler State
forwardHandler (Router router) =
  let
    view address state parsed = Nothing
  in
    {
      view = view,
      inputs = [
        router.forward Route.Error
      ]
    }
