module Handler.Widgets where

import Dict
import String
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

homeHeader : Router Route State -> Locale -> Html
homeHeader = lazy2 <| \router locale ->
  let
    version = span [class "version"] [text <| Locale.i18n locale "alpha" []]
  in
    Html.header [class "main"] [
      Html.h1 [class "title"] [homeLink router locale config.title, version],
      Html.h2 [class "subtitle"] [text <| Locale.i18n locale "Travel in photography" []]
    ]

innerHeader : Router Route State -> Locale -> Html -> Html
innerHeader = lazy3 <| \router locale title ->
  let
    (Router r) = router
    homeText = Locale.i18n locale "Home" []
    -- title' = Html.a (r.bindForward (Routes.Home, Dict.fromList [("locale", Locale.toString locale)]) [])
    --   [text title]
  in
    Html.header [class "main"] [
      Html.h1 [class "title"] [homeLink router locale homeText, text " / ", title]
    ]

footer : Router Route State -> Locale -> Html
footer router locale =
  let
    (Router r) = router
    about    = Html.a (r.bindForward (Routes.Static "about",    Dict.fromList [("locale", Locale.toString locale)]) []) [text <| Locale.i18n locale "About" []]
    contacts = Html.a (r.bindForward (Routes.Static "contacts", Dict.fromList [("locale", Locale.toString locale)]) []) [text <| Locale.i18n locale "Contacts" []]
    sep = text " | "
  in Html.footer [] [
    homeLink router locale (String.toLower config.title),
    sep, text <| Locale.i18n locale "© 2015, Artem Puchenkin" [],
    sep, about,
    sep, contacts
  ]

{-| Links -}

homeLink : Router Route State -> Locale -> String -> Html
homeLink =
  lazy3 <| \(Router router) locale title ->
    Html.a (router.bindForward (Routes.Home, Dict.fromList [("locale", Locale.toString locale)]) []) [text title]

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

categoryLink : Router Route State -> Category -> Locale -> Html
categoryLink = lazy3 <| \ (Router router) (Category category) locale ->
  let
    params = case category.parent of
      Just (Right (Category pc)) -> [("category", pc.name), ("subcategory", category.name)]
      _ -> [("category", category.name)]
    params' = ("locale", Locale.toString locale) :: params
  in
    Html.a (router.bindForward (Routes.Category, Dict.fromList params') [class "active"]) [Html.text category.title]

photoLink : Router Route State -> Photo -> RouteParams -> Html
photoLink  = lazy3 <| \ (Router router) photo params ->
    Html.a (router.bindForward (Routes.Photo, Dict.insert "photo" (toString photo.id) params) []) [Html.text photo.src]
