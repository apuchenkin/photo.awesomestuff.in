module Handler.Widgets where

import Dict
import Handler.Config exposing (..)
import Html exposing (Html, text, span)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Html.Attributes exposing (class, classList, hreflang)

import Either exposing (Either (..))
import Handler.Routes as Routes exposing (Route)
import Handler.Actions exposing (..)
import Lib.Types exposing (Router (..), RouteParams)
import Handler.Locale as Locale exposing (Locale)

loader : Bool -> Html
loader = lazy <| \visible ->
  let attributes = classList [
      ("hidden", not visible),
      ("loader", True)
    ]
  in Html.div [attributes] []

header : Router Route State -> Locale -> Html
header = lazy2 <| \(Router router) locale ->
  let
    version = span [class "version"] [text <| Locale.i18n locale "alpha" []]
    homeLink = Html.a (router.bindForward (Routes.Home, Dict.fromList [("locale", Locale.toString locale)]) []) [text config.title]
  in
    Html.header [class "main"] [
      Html.h1 [class "title"] [homeLink, version],
      Html.h2 [class "subtitle"] [text <| Locale.i18n locale "Travel in photography" []]
    ]

{-| Links -}

homeLink : Router Route State -> RouteParams -> Locale -> Html
homeLink =
  lazy3 <| \(Router router) params locale ->
    let
      homeText = Locale.i18n locale "HOME" []
    in Html.a (router.bindForward (Routes.Home, params) []) [Html.text homeText]

languageSelector :  Router Route State -> State -> Html
languageSelector = \(Router router) state ->
  let
    route = Maybe.withDefault Routes.Home state.router.route
    params locale = flip Dict.union state.router.params <| Dict.fromList [("locale", Locale.toString locale)]
    attributes locale = [
      classList [("active", locale == state.locale)],
      hreflang (Locale.toString locale)
    ]
  in Html.div [class "language"]
  <| flip List.map Locale.locales
  <| \ locale -> Html.a (router.bindForward (route, params locale) (attributes locale)) [Html.text <| Locale.toString locale]

categoryLink : Router Route State -> Category -> RouteParams -> Html
categoryLink = lazy3 <| \ (Router router) (Category category) dict ->
  let
    params = [("category", category.name)]
    params' = Dict.fromList <| case category.parent of
      Nothing -> params
      Just p -> case p of
        Left _ -> params
        Right (Category pc) ->  [("category", pc.name), ("subcategory", category.name)]
  in Html.a (router.bindForward (Routes.Category, Dict.union params' dict) []) [Html.text category.title]

photoLink : Router Route State -> Photo -> RouteParams -> Html
photoLink  = lazy3 <| \ (Router router) photo params ->
    Html.a (router.bindForward (Routes.Photo, Dict.insert "photo" (toString photo.id) params) []) [Html.text photo.src]
