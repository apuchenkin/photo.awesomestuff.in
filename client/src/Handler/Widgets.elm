module Handler.Widgets where

import Dict
import String
import Date
import Random
import Time exposing (Time)
import Html exposing (Html, text, span)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Html.Attributes as Attr exposing (class, classList, hreflang)
import Html.Events as Events
import Either exposing (Either (..))
import Json.Decode  as Json
import List.Extra as List'
import Router.Helpers exposing (singleton, noFx)
import Router.Types exposing (Router, RouteParams, Response (..), Action)

import App.Config exposing (..)
import App.Actions exposing (stopLoading)
import App.Routes as Routes exposing (Route)
import App.Model exposing (..)
import App.Actions exposing (..)
import App.Locale as Locale exposing (Locale)
import App.Resolutions exposing (adjust)
import Service.Photo exposing (..)

loader : Bool -> Html
loader = lazy <| \visible ->
  let attributes = classList [
      ("hidden", not visible),
      ("loader", True)
    ]
  in Html.div [attributes] [Html.div [Attr.class "accent"] []]

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

galleriesWidget : Router Route State -> List Category -> Locale -> Html
galleriesWidget router categories locale = Html.div [class "galleries"] [
  Html.h2 [] [text <| Locale.i18n locale "Galleries" []],
  Html.ul []
      <| List.map (\c -> Html.li [] [categoryWidget router c locale])
      <| List.filter (\(Category c) -> c.parent == Nothing) categories
    ]

categoryWidget : Router Route State -> Category -> Locale -> Html
categoryWidget router category locale =
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
        (flip Maybe.map c.image <| \image -> Html.img [Attr.width config.gallery.width, Attr.src (config.staticEndpoint ++ image), Attr.alt c.title] []),
        (flip Maybe.map c.date <| \date -> Html.span [class "sub"] [text <| dateText date])
      ]
    aside = Html.aside [] [
        Html.h3 [] [categoryLink router category locale True],
        Html.ul [] (List.map (\child -> Html.li [] [categoryLink router child locale False]) c.childs)
      ]
  in
    Html.div [class "gallery"] [
      cover,
      aside
    ]

gallery : Router Route State -> RouteParams -> List Photo -> Time -> Html
gallery router params photos time =
  let
    seed = Random.initialSeed <| floor <| Time.inSeconds time
    photos' = remapPhotos seed photos
  in Html.div [Attr.class "gallery"]
  <| singleton <| Html.ul [] <| flip List.map photos'
  <| \photo -> Html.li [] [brickWidget router params photo]

brickWidget : Router Route State -> RouteParams -> Photo -> Html
brickWidget router params photo =
    let
      (w,h) = (photo.width, photo.height)
      ratio = photo.ratio
      inc = if ratio >= 1 then ratio else 1 / ratio
      (m1,m2) = if w < h then (ceiling <| toFloat w * inc, h) else (ceiling <| toFloat h * inc, w)
      s = max m1 m2
      filename = Maybe.withDefault "photo.jpg" <| List'.last <| String.split "/" photo.src
      src = config.apiEndpoint ++ "/hs/photo/" ++ toString photo.id ++ "/" ++ toString s ++ "/" ++ toString s ++ "/" ++ filename
      content = Html.div [Attr.class "brick", Attr.style [("width", toString w ++ "px"), ("height", toString h ++ "px"),("background-image", "url(" ++ src ++ ")")]] []
    in photoLink router photo params content

photoWidget : Router Route State -> Signal.Address (Action State) -> RouteParams -> Photo -> (Int, Int) -> (Int, Int) -> Bool -> Locale -> Html
photoWidget router address params photo (prev, next) (w,h) isLoading locale =
    let
      (w', h') = adjust (w - 40, h - 40)
      onLoad = Events.on "load" Json.value <| always <| Signal.message address stopLoading
      filename = Maybe.withDefault "photo.jpg" <| List'.last <| String.split "/" photo.src
      src = config.apiEndpoint ++ String.join "/" ["", "hs", "photo", toString photo.id, toString w', toString h', filename]
      image = Html.img (router.bindForward (Routes.Photo, Dict.union (Dict.fromList [("photo", toString next)]) params) [Attr.class "photo", Attr.src src, onLoad]) []
      caption = flip Maybe.map photo.caption <| \c -> Html.span [Attr.class "caption"] [Html.text c]
      author = flip Maybe.map photo.author <| \author -> Html.div [] [Html.text <| Locale.i18n locale "author " [], Html.span [Attr.class "author"] [Html.text author.name]]
    in
      Html.div (router.bindForward (Routes.Category, params) [classList [("photo-widget", True)]]) [
        Html.figure [classList [("content", True), ("hidden", isLoading)]] [
          Html.div [Attr.class "tools"] [Html.a (router.bindForward (Routes.Category, params) []) <| [Html.text <| Locale.i18n locale "CLOSE" [], Html.text " ", Html.i [Attr.class "icon-cancel"] []]]
        , image
        , Html.figcaption [Attr.class "description"] <| List.filterMap identity [caption, author]
        ]
      , Html.a (router.bindForward (Routes.Photo, Dict.union (Dict.fromList [("photo", toString prev)]) params) [classList [("nav", True), ("prev", True)], Attr.title <| Locale.i18n locale "PREV" []])
          <| [Html.i [Attr.class "icon-left-open"] []]
      , Html.a (router.bindForward (Routes.Photo, Dict.union (Dict.fromList [("photo", toString next)]) params) [classList [("nav", True), ("next", True)], Attr.title <| Locale.i18n locale "NEXT" []])
          <| [Html.i [Attr.class "icon-right-open"] []]
      ]
    --
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
