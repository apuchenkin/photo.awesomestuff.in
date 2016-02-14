module Handler.Widgets where

import Dict
import Html exposing (Html)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Html.Attributes exposing (class, classList)

import Either exposing (Either (..))
import Handler.Routes as Routes exposing (Route)
import Handler.Actions exposing (..)
import Lib.Types exposing (Router (..), RouteParams)
import Handler.Locale as Locale

homeLink : Router Route State -> RouteParams -> Html
homeLink = lazy2 <| \(Router router) params -> Html.a (router.bindForward (Routes.Home, params) []) [Html.text "HOME"]

loader : Bool -> Html
loader = lazy <| \visible ->
  let attributes = classList [
      ("hidden", not visible),
      ("loader", True)
    ]
  in Html.div [attributes] []

languageSelector :  Router Route State -> State -> Html
languageSelector = \(Router router) state ->
  let
    route = Maybe.withDefault Routes.Home state.router.route
    params locale = flip Dict.union state.router.params <| Dict.fromList [("locale", Locale.toString locale)]
    attributes locale = classList [("active", locale == state.locale)]
  in Html.div [class "language"]
  <| flip List.map Locale.locales
  <| \ locale -> Html.a (router.bindForward (route, params locale) [attributes locale]) [Html.text <| Locale.toString locale]

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
    Html.a (router.bindForward (Routes.Photo, Dict.insert "photo" (toString photo.id) params) []) [Html.text photo.src]
