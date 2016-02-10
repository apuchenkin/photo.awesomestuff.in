module Handler.Default where

import Dict
import Either exposing (Either (..))
import Lib.Helpers exposing (noFx, singleton)
import Handler.Actions exposing (..)
import Lib.Types exposing (Router (..), Handler, Response (..))

import Html exposing (div, text, Html, button)
import Html.Attributes exposing (href,class)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Handler.Routes as Route exposing (Route)

categoryLink : Category -> Router Route State -> Html
categoryLink (Category category) (Router router) =
  let
    params = [("category", category.name)]
    params' = case category.parent of
      Nothing -> params
      Just p -> case p of
        Left _ -> params
        Right (Category pc) ->  [("category", pc.name), ("subcategory", category.name)]
  in Html.a (router.bindForward (Route.Category, Dict.fromList params') []) [text category.title]

photoLink : Photo -> Router Route State -> Html
photoLink photo (Router router) = Html.a (router.bindForward (Route.Photo, Dict.fromList [("photo", toString photo.id)]) []) [text photo.src]

homeHandler : Router Route State -> Handler State
homeHandler (Router router) =
  let
    loader = Html.div [class "loader"] []
    view address state parsed =
      let
        -- _ = Debug.log "homeHandler" state
        home = lazy (always <| Html.a (router.bindForward (Route.Home,Dict.empty) []) [text "HOME"]) ()
        categories c = Html.div [class "categories"] <| List.map (\c -> lazy2 categoryLink (snd c) (Router router)) <| Dict.toList c
        rest = case parsed of
          Nothing   -> [lazy categories state.categories]
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
    view address state parsed =
      let
        -- _ = Debug.log "categoryHandler" state
        category = getCategory state

        photos =  lazy (\l -> div [] <| List.map (\p -> lazy2 photoLink p (Router router)) l) state.photos
        parsed' = div [] <| Maybe.withDefault [] <| Maybe.map singleton parsed

      in Maybe.map (\c -> div [] [text <| toString c, photos, parsed'] ) category
  in
    {
      view = view,
      inputs = [loadPhotos]
    }

photoHandler : Router Route State -> Handler State
photoHandler (Router router) =
  let
    view address state _ =
      let
        -- _ = Debug.log "photoHandler" state
        photo = Dict.get "photo" state.router.params
      in Maybe.map (\p -> div [] [text <| toString p] ) photo
  in
    {
      view = view,
      inputs = [
        loadPhoto
      ]
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
