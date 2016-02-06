module Handler.Default where

import Dict
import Either exposing (Either (..))
import Lib.Helpers exposing (noFx)
import Handler.Actions exposing (..)
import Lib.Types exposing (Router (..), Handler, Response (..))

import Html exposing (div, text, Html, button)
import Html.Attributes exposing (href,class)
import Handler.Routes as Route exposing (Route)

homeHandler : Router Route State -> Handler State
homeHandler (Router router) =
  let
    loader = Html.div [class "loader"] []

    categoryLink state (Category category) =
      let
      params = [("category", category.name)]
      params' = case category.parent of
        Nothing -> params
        Just p -> case p of
          Left _ -> params
          Right (Category pc) ->  [("category", pc.name), ("subcategory", category.name)]

      in Html.a (router.bindForward state (Route.Category, Dict.fromList params') []) [text category.title]

    view address state parsed =
      let
        -- _ = Debug.log "homeHandler" state
        home = Html.a (router.bindForward state (Route.Home,Dict.empty) []) [text "HOME"]
        rest = case parsed of
          Nothing   -> [Html.div [class "categories"] <| List.map (categoryLink state << snd) <| Dict.toList state.categories]
          Just html -> [html]
      in Just <| div [] <| case state.isLoading of
        True  -> loader :: home :: rest
        False -> home :: rest
  in
    {
      view = view,
      inputs = [loadCategories]
    }

categoryHandler : Router Route State -> Handler State
categoryHandler (Router router) =
  let
    view address state _ =
      let
        _ = Debug.log "categoryHandler" state
        category = Dict.get "category" state.router.params
        category' = case Dict.get "subcategory" state.router.params of
          Nothing -> category
          c -> c

        mc =  Maybe.map (flip Dict.get state.categories) category'
      in Maybe.map (\c -> div [] [text <| toString c]) mc
  in
    {
      view = view,
      inputs = [loadPhotos]
    }

notFoundHandler : Router Route State -> Handler State
notFoundHandler _ =
  let
    view address state parsed = Just <| div [] [text <| "404"]
  in
    {
      view = view,
      inputs = []
    }
