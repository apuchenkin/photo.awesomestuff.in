module Handler.Widgets where

import Dict
import Html exposing (Html)
import Either exposing (Either (..))
import Handler.Routes as Routes exposing (Route)
import Handler.Actions exposing (..)
import Lib.Types exposing (Router (..), RouteParams)
import Html.Lazy exposing (lazy, lazy2, lazy3)

homeLink : Router Route State -> RouteParams -> Html
homeLink = lazy2 <| \(Router router) params -> Html.a (router.bindForward (Routes.Home, params) []) [Html.text "HOME"]

categoryLink : Router Route State -> Category -> RouteParams -> Html
categoryLink = lazy3 <| \ (Router router) (Category category) dict ->
  let
    params = [("category", category.name)]
    params' = Dict.fromList <| case category.parent of
      Nothing -> params
      Just p -> case p of
        Left _ -> params
        Right (Category pc) ->  [("category", pc.name), ("subcategory", category.name)]
  in Html.a (router.bindForward (Routes.Category, Dict.union dict params') []) [Html.text category.title]

photoLink : Router Route State -> Photo -> RouteParams -> Html
photoLink  = lazy3 <| \ (Router router) photo params ->
  -- let
  --   (locale, category, subcategory) = args
  --   params = Dict.fromList [("locale", locale),("category", category), ("subcategory", subcategory)]
  -- in
  Html.a (router.bindForward (Routes.Photo, Dict.insert "photo" (toString photo.id) params) []) [Html.text photo.src]
