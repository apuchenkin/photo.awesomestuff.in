module Handler.Widgets where

import Dict
import String
import Date

import Random
import Html exposing (Html, text, span)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Html.Attributes as Attr exposing (class, classList, hreflang)
import Either exposing (Either (..))
import List.Extra as List'
import Router.Types exposing (Router, RouteParams)

import App.Config exposing (..)
import App.Routes as Routes exposing (Route)
import App.Actions exposing (..)
import App.Locale as Locale exposing (Locale)

loader : Bool -> Html
loader = lazy <| \visible ->
  let attributes = classList [
      ("hidden", not visible),
      ("loader", True)
    ]
  in Html.div [attributes] []

languageSelector :  Router Route State -> State -> Html
languageSelector = \router state ->
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
    about    = Html.a (router.bindForward (Routes.Static "about",    Dict.fromList [("locale", Locale.toString locale)]) []) [text <| Locale.i18n locale "About" []]
    contacts = Html.a (router.bindForward (Routes.Static "contacts", Dict.fromList [("locale", Locale.toString locale)]) []) [text <| Locale.i18n locale "Contacts" []]
    sep = text " | "
  in Html.footer [] [
    homeLink router locale (String.toLower config.title),
    sep, text <| Locale.i18n locale "© 2015, Artem Puchenkin" [],
    sep, about,
    sep, contacts
  ]

categoryWidget : Router Route State -> Category -> List Category -> Locale -> Html
categoryWidget router category childs locale =
  let
    (Category c) = category
    params = case c.parent of
      Just (Right (Category pc)) -> [("category", pc.name), ("subcategory", c.name)]
      _ -> [("category", c.name)]
    params' = ("locale", Locale.toString locale) :: params

    dateText date = Locale.i18n locale "{0}, {1}" [
        Locale.i18n locale (toString <| Date.month date) [],
        toString (Date.year date)
      ]

    cover = Html.a (router.bindForward (Routes.Category, Dict.fromList params') [class "cover"])
      <| List.filterMap identity [
        (flip Maybe.map c.image <| \image -> Html.img [Attr.src (config.staticEndpoint ++ image), Attr.alt c.title] []),
        (flip Maybe.map c.date <| \date -> Html.span [class "sub"] [text <| dateText date])
      ]
    aside = Html.aside [] [
        Html.h3 [] [categoryLink router category locale True],
        Html.ul [] (List.map (\c -> Html.li [] [categoryLink router c locale False]) childs)
      ]
  in
    Html.div [class "gallery"] [
      cover,
      aside
    ]

gallery : Router Route State -> RouteParams -> List Photo -> Html
gallery router params photos =
  let
    views = List.map .views photos
    avg = (toFloat <| List.sum views) / (toFloat <| List.length views)
    max = Maybe.withDefault 0 <| List.maximum views
    g = config.gutter
    w = config.brickWidth

    s = w
    s2 = (s * 2) + g
    s3 = (s * 3) + (g * 2)
    s4 = (s * 4) + (g * 3)

    dsmap mode ratio isHorisontal = case mode of
      1 -> if isHorisontal then (if ratio >= 3 then s3 else s2, s) else (s, s2)
      2 -> if ratio >= 4 then (s4, s) else (if ratio >= 2 then s3 else s2, s2)
      3 -> if isHorisontal then (if ratio >= 2 then s4 else s3, s2) else (s2, s3)
      _ -> (if ratio >= 2 then s2 else s, s)

  in Html.div [Attr.class "gallery"] <| flip List.map photos
  <| \photo ->
    let
      v = toFloat photo.views
      std = (v - avg) ^ 2
      pow = (toFloat max) / sqrt (std * v)
      norm  = List.map ((*) max) [8,4,2,1]
      norm' = List.map (\r -> floor <| r * pow ^ 2) [1,2,3,4]
      prob = List.map2 (+) norm norm'
      prob' = List.map (\p -> p // (Maybe.withDefault 1 <| List.minimum prob)) prob
      probList = List.concat <| List.map2 (\m p -> List.repeat p m) [1,2,3,4] prob'
      gen = Random.int 0 (List.length probList - 1)
      mode = Maybe.withDefault 1 <| List'.getAt probList (fst <| Random.generate gen (Random.initialSeed 0))
      dims = dsmap mode 1.6 True

    in photoWidget router photo params dims

photoWidget : Router Route State -> Photo -> RouteParams -> (Int, Int) -> Html
photoWidget router photo params (w, h) =
    let content = Html.img [Attr.src (config.apiEndpoint ++ "hs/photo/" ++ toString photo.id ++ "/" ++ toString w ++ "/" ++ toString h ++ "/static")] []
    in photoLink router photo params content

{-| Links -}

homeLink : Router Route State -> Locale -> String -> Html
homeLink =
  lazy3 <| \router locale title ->
    Html.a (router.bindForward (Routes.Home, Dict.fromList [("locale", Locale.toString locale)]) []) [text title]

photoLink : Router Route State -> Photo -> RouteParams -> Html -> Html
photoLink = -- lazy3 <| \
  \ router photo params content ->
    Html.a (router.bindForward (Routes.Photo, Dict.insert "photo" (toString photo.id) params) []) [content]

categoryLink : Router Route State -> Category -> Locale -> Bool -> Html
categoryLink router (Category category) locale isActive =
  let
    params = case category.parent of
      Just (Right (Category pc)) -> [("category", pc.name), ("subcategory", category.name)]
      _ -> [("category", category.name)]
    params' = ("locale", Locale.toString locale) :: params
    attributes = [
      classList [("active", isActive)]
    ]
  in
    Html.a (router.bindForward (Routes.Category, Dict.fromList params') attributes) [Html.text category.title]
