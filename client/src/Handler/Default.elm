module Handler.Default where

import Handler.Actions exposing (..)
import Lib.Router exposing (Router (..), Handler, Response (..), noFx)
import Html exposing (div, text, Html, button)
import Effects  exposing (Effects, Never)
import Html.Attributes exposing (href,class)
import Handler.Routes as Route exposing (Route)
import List.Extra as L

homeHandler : Router Route State -> Handler State
homeHandler (Router router) =
  let
    -- _ = Debug.log "homeHandler" state
    home = Html.a [href "/"] [text "HOME"]
    loader = Html.div [class "loader"] []
    view address state parsed =
      let
        rest = case parsed of
          Nothing   -> List.map (\c -> Html.a ((router.bindForward (Route.Category c.name)) []) [text c.title]) state.categories
          Just html -> [html]
      in Just <| div [] <| case state.isLoading of
        True  -> loader :: home :: rest
        False -> home :: rest
  in
    {
      view = view,
      inputs = [
        loadCategories
        -- (\state -> let _ = Debug.log "categoryi" state in Response (state, Nothing))
      ]
    }

categoryHandler : Router Route State -> String -> Handler State
categoryHandler (Router router) category =
  let
    view address state _ =
      let
        _ = Debug.log "categoryHandler" state
        mc = L.find (\c -> c.name == category) state.categories
      in Maybe.map (\c -> div [] [text <| toString c]) mc
  in
    {
      view = view,
      inputs = [
        -- (\state -> let _ = Debug.log "categoryHandler: i1" state in Response (noFx state))
        -- (\state -> let _ = Debug.log "categoryi" state in case List.filter (\c -> c.name == category) state.categories of
        --   []         -> Response (state, Nothing)
        --   [category] -> Response (state, Nothing)
        --   _          -> Response (state, Nothing) --router.forward Route.Error state
        -- )
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
