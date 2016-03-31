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
import Router.Helpers exposing (singleton, noFx, chainAction)
import Router.Types exposing (Router, RouteParams, Response (..), Action)

import App.Config exposing (..)
import App.Routes as Routes exposing (Route)
import App.Model exposing (..)
import App.Actions exposing (..)
import App.Locale as Locale exposing (Locale)
import Service.Resolutions exposing (adjust)
import Service.Photo exposing (..)

loader : Bool -> Bool -> Html
loader = lazy2 <| \visible transition ->
  let attributes = classList [
      ("hidden", not visible)
    , ("transition", transition)
    , ("loader", True)
    ]
  in Html.div [attributes, Attr.key "loader"] [Html.div [Attr.class "accent"] []]

languageSelector :  Router Route State -> Maybe Route -> RouteParams -> Locale -> Html
languageSelector router = lazy3 <| \route params locale ->
  let
    route' = Maybe.withDefault Routes.Home route
    params' loc = Dict.union (Dict.fromList [("locale", Locale.toString loc)]) params
    attributes loc = [
      classList [("active", locale == loc)],
      hreflang (Locale.toString loc)
    ]
  in Html.div [class "language", Attr.key "language-selector"]
  <| flip List.map Locale.locales
  <| \loc -> Html.a (router.bindForward (route', params' loc) (attributes loc)) [Html.text <| Locale.toString loc]

homeHeader : Router Route State -> Locale -> Html
homeHeader = lazy2 <| \router locale ->
  let
    version = span [class "version"] [text <| Locale.i18n locale "ALFA" []]
  in
    Html.header [class "main", Attr.key "header-home"] [
      Html.h1 [class "title"] [homeLink router locale config.title, version],
      Html.h2 [class "subtitle"] [text <| Locale.i18n locale "SUBTITLE" []]
    ]

innerHeader : Router Route State -> Locale -> Html -> Html
innerHeader = lazy3 <| \router locale title ->
  let
    homeText = Locale.i18n locale "Home" []
  in
    Html.header [class "main", Attr.key "header-inner"] [
      Html.h1 [class "title"] [homeLink router locale homeText, text " / ", title]
    ]

footer : Router Route State -> Locale -> Html
footer = lazy2 <| \router locale ->
  let
    about    = Html.a (router.bindForward (Routes.Static "about",    Dict.fromList [("locale", Locale.toString locale)]) []) [text <| Locale.i18n locale "ABOUT" []]
    contacts = Html.a (router.bindForward (Routes.Static "contacts", Dict.fromList [("locale", Locale.toString locale)]) []) [text <| Locale.i18n locale "CONTACTS" []]
    sep = text " | "
  in Html.footer [Attr.key "footer"] [
    homeLink router locale (String.toLower config.title),
    sep, text <| Locale.i18n locale "© 2015, Artem Puchenkin" [],
    sep, about,
    sep, contacts
  ]

navigation : Router Route State -> Locale -> Maybe Category -> Maybe Category -> Html
navigation router =
  let
    categoryLink' = categoryLink router
  in
    lazy3 <| \locale category subcategory ->
      Html.nav [Attr.key "navigation", Attr.class "categories"] [
        Html.ul []
          <| List.map    (\c -> Html.li [] [categoryLink' c locale (Just c == subcategory)])
          <| Maybe.withDefault []
          <| flip Maybe.map category <| \(Category c) -> c.childs
      ]


galleriesWidget : Router Route State -> List Category -> Locale -> Html
galleriesWidget = lazy3 <| \router categories locale -> Html.div [class "galleries", Attr.key "galleries"] [
  Html.h2 [] [text <| Locale.i18n locale "Galleries" []],
  Html.ul []
      <| List.map (\c -> Html.li [] [categoryWidget router c locale])
      <| List.filter (\(Category c) -> c.parent == Nothing) categories
    ]

categoryWidget : Router Route State -> Category -> Locale -> Html
categoryWidget = lazy3 <| \router category locale ->
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
    Html.div [class "gallery", Attr.key "gallery-brick"] [
      cover,
      aside
    ]

gallery : Router Route State -> Locale -> List Photo -> Time -> Html
gallery router = lazy3 <| \ locale photos time ->
  let
    brick = brickWidget router locale
    seed = Random.initialSeed <| floor <| Time.inSeconds time
    photos' = remapPhotos seed photos
  in Html.div [Attr.class "gallery", Attr.key "gallery"]
  <| singleton <| Html.ul [] <| flip List.map photos'
  <| \photo -> Html.li [] [brick photo]

brickWidget : Router Route State -> Locale -> Photo -> Html
brickWidget router locale photo =
    let
      (w,h) = (photo.width, photo.height)
      ratio = photo.ratio
      inc = if ratio >= 1 then ratio else 1 / ratio
      (m1,m2) = if w < h then (ceiling <| toFloat w * inc, h) else (ceiling <| toFloat h * inc, w)
      s = max m1 m2
      filename = Maybe.withDefault "photo.jpg" <| List'.last <| String.split "/" photo.src
      src = config.apiEndpoint ++ "/hs/photo/" ++ toString photo.id ++ "/" ++ toString s ++ "/" ++ toString s ++ "/" ++ filename
      content = Html.div [Attr.class "brick", Attr.style [("width", toString w ++ "px"), ("height", toString h ++ "px"),("background-image", "url(" ++ src ++ ")")]] []
    in photoLink router photo locale content

photoWidget : Router Route State -> RouteParams -> Photo -> (Int, Int) -> (Int, Int) -> Locale -> Bool -> Html
photoWidget router params photo (prev, next) (w,h) locale transition =
    let
      (w', h') = adjust (w - 40, h - 40)
      loadAction state = let photo' = {photo | isLoaded = True} in Response <| noFx {state | photo = Just photo'}
      onLoad = Events.on "load" Json.value <| always <| Signal.message router.address loadAction
      filename = Maybe.withDefault "photo.jpg" <| List'.last <| String.split "/" photo.src
      src = config.apiEndpoint ++ String.join "/" ["", "hs", "photo", toString photo.id, toString w', toString h', filename]

      bindExit : List Html.Attribute -> List Html.Attribute
      bindExit attrs =
        let
          route = (Routes.Category, params)
          options = {stopPropagation = True, preventDefault = True}
          action _ = Signal.message router.address <| withTransition Out <| (\state -> Response <| noFx {state | photo = Nothing}) `chainAction` (router.forward route)
        in
          Attr.href (router.buildUrl route)
          :: Events.onWithOptions "click" options Json.value action
          :: attrs

      image = Html.img (router.bindForward (Routes.Photo, Dict.union (Dict.fromList [("photo", toString next)]) params) [Attr.class "photo", Attr.src src, Attr.style [("max-height", toString (h - 120) ++ "px")],onLoad]) []
      caption = flip Maybe.map photo.caption <| \c -> Html.span [Attr.class "caption"] [Html.text c]
      author = flip Maybe.map photo.author <| \author -> Html.div [] [Html.text <| Locale.i18n locale "author " [], Html.span [Attr.class "author"] [Html.text author.name]]
    in
      Html.div (bindExit [classList [("photo-widget", True), ("transition", transition)], Attr.key "photo-widget"]) [
        loader (not photo.isLoaded) False
      , Html.figure [classList [("content", True), ("hidden", not photo.isLoaded)]] [
          Html.div [Attr.class "tools"] [Html.a (bindExit []) <| [Html.text <| Locale.i18n locale "CLOSE" [], Html.text " ", Html.i [Attr.class "icon-cancel"] []]]
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

photoLink : Router Route State -> Photo -> Locale -> Html -> Html
photoLink router photo locale content =
    let
      categoryParams = Maybe.withDefault []
        <| flip Maybe.map photo.category
        <| \(Category c) -> Maybe.withDefault [("category", c.name)] <| flip Maybe.map c.parent
        <| \p -> Either.elim (always [("category", c.name)]) (\(Category p') -> [("category", p'.name),("subcategory", c.name)]) p

      params = Dict.fromList <| [
        ("photo", toString photo.id)
      , ("locale", Locale.toString locale)
      ] ++ categoryParams
    in
      Html.a (router.bindForward (Routes.Photo, params) []) [content]

categoryLink : Router Route State -> Category -> Locale -> Bool -> Html
categoryLink router = lazy3 <| \ (Category category) locale isActive ->
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
