module Handler.Default where

import Dict
import Lib.Helpers exposing (noFx)
import Handler.Actions exposing (..)
import Lib.Types exposing (Router (..), Handler, Response (..))

import Html exposing (div, text, Html, button)
import Html.Attributes exposing (href,class)
import Handler.Routes as Route exposing (Route)
import List.Extra as L

homeHandler : Router Route State -> Handler State
homeHandler (Router router) =
  let
    -- _ = Debug.log "homeHandler" state

    loader = Html.div [class "loader"] []
    view address state parsed =
      let
        -- _ = Debug.log "homeHandler" state
        home = Html.a (router.bindForward state (Route.Home,Dict.empty) []) [text "HOME"]
        rest = case parsed of
          Nothing   -> [Html.div [class "categories"] <| List.map (\c -> Html.a (router.bindForward state (Route.Category, Dict.fromList [("category", c.name)]) []) [text c.title]) state.categories]
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

categoryHandler : Router Route State -> Handler State
categoryHandler (Router router) =
  let
    view address state _ =
      let
        _ = Debug.log "categoryHandler" state
        mc = L.find (\c -> Just c.name == Dict.get "category" state.router.params) state.categories
      in Maybe.map (\c -> div [] [text <| toString c]) mc
  in
    {
      view = view,
      inputs = [
        (\state -> let _ = Debug.log "categoryHandler: i1" state in Response (noFx state))
        -- (\state -> let _ = Debug.log "categoryi" state in case List.filter (\c -> c.name == category) state.categories of
        --   []         -> Response (state, Nothing)
        --   [category] -> Response (state, Nothing)
        --   _          -> Response (state, Nothing) --router.forward Route.Error state
        -- )
      ]
    }


errorHandler : Router Route State -> Handler State
errorHandler _ =
  let
    view address state parsed = Just <| div [] [text <| "404"]
  in
    {
      view = view,
      inputs = []
    }

forwardHandler : Router Route State -> Handler State
forwardHandler (Router router) =
  let
    view address state parsed = Nothing
  in
    {
      view = view,
      inputs = [
        router.forward (Route.Error, Dict.empty)
      ]
    }
