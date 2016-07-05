module Handler.Widgets exposing (..)

import Dict
import String
import Random
import Time exposing (Time)
import Html exposing (Html, text, span)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Html.Attributes as Attr exposing (class, classList, hreflang)
import Html.Events as Events
import Either exposing (Either (..))
import Json.Decode  as Json
import List.Extra as List'
import Router.Helpers exposing (noFx, chainAction)
import Router.Types exposing (Router, RouteParams, Response (..), Action)

import App.Config exposing (..)
import App.Routes as Routes exposing (Route)
import App.Model exposing (..)
import App.Actions exposing (..)
import App.Locale as Locale exposing (Locale)
import Service.Resolutions exposing (adjust)
import Service.Photo exposing (..)

{-| Wraps something in a list -}
singleton : a -> List a
singleton action = [ action ]

loader : Bool -> Bool -> Html (Action State)
loader = lazy2 <| \visible transition ->
  let attributes = classList [
      ("hidden", not visible)
    , ("transition", transition)
    , ("loader", True)
    ]
  in Html.div [attributes] [Html.div [Attr.class "accent"] []]

languageSelector :  Router Route State -> Maybe Route -> RouteParams -> Locale -> Html (Action State)
languageSelector router = lazy3 <| \route params locale ->
  let
    route' = Maybe.withDefault Routes.Home route
    params' loc = Dict.union (Dict.fromList [("locale", Locale.toString loc)]) params
    attributes loc = [
      classList [("active", locale == loc)],
      hreflang (Locale.toString loc)
    ]
  in Html.div [class "language"]
  <| flip List.map Locale.locales
  <| \loc -> Html.a (router.bindForward (route', params' loc) (attributes loc)) [Html.text <| Locale.toString loc]

homeHeader : Router Route State -> Locale -> Html (Action State)
homeHeader = lazy2 <| \router locale ->
  let
    version = span [class "version"] [text <| Locale.i18n locale <| Locale.Alfa]
  in
    Html.header [class "main"] [
      Html.h1 [class "title"] [homeLink router locale config.title, version],
      Html.h2 [class "subtitle"] [text <| Locale.i18n locale <| Locale.Subtitle]
    ]

innerHeader : Router Route State -> Locale -> Html (Action State) -> Html (Action State)
innerHeader = lazy3 <| \router locale title ->
  let
    homeText = Locale.i18n locale <| Locale.Home
  in
    Html.header [class "main"] [
      Html.h1 [class "title"] [homeLink router locale homeText, text " / ", title]
    ]

footer : Router Route State -> Locale -> Html (Action State)
footer = lazy2 <| \router locale ->
  let
    about    = Html.a (router.bindForward (Routes.Static "about",    Dict.fromList [("locale", Locale.toString locale)]) []) [text <| Locale.i18n locale <| Locale.About Locale.AboutTitle]
    contacts = Html.a (router.bindForward (Routes.Static "contacts", Dict.fromList [("locale", Locale.toString locale)]) []) [text <| Locale.i18n locale <| Locale.Contacts Locale.ContactsTitle]
    sep = text " | "
  in Html.footer [] [
    homeLink router locale (String.toLower config.title),
    sep, text <| Locale.i18n locale <| Locale.Copy,
    sep, about,
    sep, contacts
  ]

navigation : Router Route State -> Locale -> Maybe Category -> Maybe Category -> Html (Action State)
navigation router =
  let
    categoryLink' = categoryLink router
  in
    lazy3 <| \locale category subcategory ->
      Html.nav [Attr.class "categories"] [
        Html.ul []
          <| List.map    (\c -> Html.li [] [categoryLink' c locale (Just c == subcategory)])
          <| Maybe.withDefault []
          <| flip Maybe.map category <| \(Category c) -> c.childs
      ]


galleriesWidget : Router Route State -> List Category -> Locale -> Html (Action State)
galleriesWidget = lazy3 <| \router categories locale -> Html.div [class "galleries"] [
  Html.h2 [] [text <| Locale.i18n locale <| Locale.Galleries ],
  Html.ul []
      <| List.map (\c -> Html.li [] [categoryWidget router c locale])
      <| List.filter (\(Category c) -> c.parent == Nothing) categories
    ]

categoryWidget : Router Route State -> Category -> Locale -> Html (Action State)
categoryWidget = lazy3 <| \router category locale ->
  let
    (Category c) = category
    params = case c.parent of
      Just (Right (Category pc)) -> [("category", pc.name), ("subcategory", c.name)]
      _ -> [("category", c.name)]
    params' = ("locale", Locale.toString locale) :: params

    dateText date = Locale.i18n locale <| Locale.Date date

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
    Html.div [class "gallery"] [--, Attr.key "gallery-brick"] [
      cover,
      aside
    ]

gallery : Router Route State -> Locale -> List Photo -> Time -> Html (Action State)
gallery router = lazy3 <| \ locale photos time ->
  let
    brick = brickWidget router locale
    seed = Random.initialSeed <| floor <| Time.inSeconds time
    photos' = remapPhotos seed photos
  in Html.div [Attr.class "gallery"] --, Attr.key "gallery"]
  <| singleton <| Html.ul [] <| flip List.map photos'
  <| \photo -> Html.li [] [brick photo]

brickWidget : Router Route State -> Locale -> Photo -> Html (Action State)
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

photoWidget : Router Route State -> RouteParams -> Photo -> (Int, Int) -> (Int, Int) -> Locale -> Bool -> Html (Action State)
photoWidget router params photo (prev, next) (w,h) locale transition =
    let
      (w', h') = adjust (w - 40, h - 40)
      loadAction state = let photo' = {photo | isLoaded = True} in Response <| noFx {state | photo = Just photo'}
      onLoad = Events.on "load" (Json.succeed loadAction)
      filename = Maybe.withDefault "photo.jpg" <| List'.last <| String.split "/" photo.src
      src = config.apiEndpoint ++ String.join "/" ["", "hs", "photo", toString photo.id, toString w', toString h', filename]

      bindExit : List (Html.Attribute (Action State)) -> List (Html.Attribute (Action State))
      bindExit attrs =
        let
          route = (Routes.Category, params)
          options = {stopPropagation = True, preventDefault = True}
          action = withTransition Out <| Just <| (\state -> Response <| noFx {state | photo = Nothing}) `chainAction` (router.forward route)
        in
          Attr.href (router.buildUrl route)
          :: Events.onWithOptions "click" options (Json.succeed action)
          :: attrs

      image = Html.img (router.bindForward (Routes.Photo, Dict.union (Dict.fromList [("photo", toString next)]) params) [Attr.class "photo", Attr.src src, Attr.style [("max-height", toString (h - 120) ++ "px")],onLoad]) []
      caption = flip Maybe.map photo.caption <| \c -> Html.span [Attr.class "caption"] [Html.text c]
      author = flip Maybe.map photo.author <| \author -> Html.div [] [Html.text <| Locale.i18n locale <| Locale.Author, Html.span [Attr.class "author"] [Html.text author.name]]
    in
      Html.div (bindExit [classList [("photo-widget", True), ("transition", transition)]]) [ --, Attr.key "photo-widget"]) [
        loader (not photo.isLoaded) False
      , Html.figure [classList [("content", True), ("hidden", not photo.isLoaded)]] [
          Html.div [Attr.class "tools"] [Html.a (bindExit []) <| [Html.text <| Locale.i18n locale <| Locale.Action Locale.Close, Html.text " ", Html.i [Attr.class "icon-cancel"] []]]
        , image
        , Html.figcaption [Attr.class "description"] <| List.filterMap identity [caption, author]
        ]
      , Html.a (router.bindForward (Routes.Photo, Dict.union (Dict.fromList [("photo", toString prev)]) params) [classList [("nav", True), ("prev", True)], Attr.title <| Locale.i18n locale <| Locale.Action Locale.Prev])
          <| [Html.i [Attr.class "icon-left-open"] []]
      , Html.a (router.bindForward (Routes.Photo, Dict.union (Dict.fromList [("photo", toString next)]) params) [classList [("nav", True), ("next", True)], Attr.title <| Locale.i18n locale <| Locale.Action Locale.Next])
          <| [Html.i [Attr.class "icon-right-open"] []]
      ]
    --
{-| Links -}
homeLink : Router Route State -> Locale -> String -> Html (Action State)
homeLink =
  lazy3 <| \router locale title ->
    Html.a (router.bindForward (Routes.Home, Dict.fromList [("locale", Locale.toString locale)]) []) [text title]

photoLink : Router Route State -> Photo -> Locale -> Html (Action State) -> Html (Action State)
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

categoryLink : Router Route State -> Category -> Locale -> Bool -> Html (Action State)
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
