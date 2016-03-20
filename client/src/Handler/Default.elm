module Handler.Default where

import String
import Dict
import Html exposing (Html)
import List.Extra exposing ((!!))
import Router.Types exposing (Router, Handler, Response (..))
import Router.Helpers exposing (doNothing)

import App.Routes as Route exposing (Route)
import App.Locale as Locale exposing (Locale)
import App.Actions exposing (..)
import App.Model exposing (..)
import Handler.Widgets exposing (..)
import Handler.Static exposing (..)

localeHandler : Router Route State -> Handler State
localeHandler router =
  let
    view state _ = Dict.empty
  in {
    view = view,
    actions = [
      resolveLocale router,
      createLinks router,
      resetMeta
    ]
  }

notFoundHandler : Router Route State -> Handler State
notFoundHandler router =
  let
    view state _ = Dict.fromList [
      ("body", notFoundWidget router state.locale)
    ]
  in {
    view = view,
    actions = [
      \state -> setTitle (Just <| Locale.i18n state.locale "ERROR" ["404"]) state
    ]
  }

staticHandler : String -> Router Route State -> Handler State
staticHandler page router =
  let
    body locale = case page of
      "about" -> aboutWidget locale
      "contacts" -> contactsWidget locale
      _ -> Html.div [] []
    title locale = Locale.i18n locale page []
    view state _ = Dict.fromList [
      ("header", innerHeader router state.locale (Html.text <| title state.locale))
    , ("body", body state.locale)
    ]
  in {
    view = view,
    actions = [
      \state -> setTitle (Just <| title state.locale) state
    ]
  }

homeHandler : Router Route State -> Handler State
homeHandler router =
  let
    view state _ = Dict.fromList [
        ("body", galleriesWidget router (Dict.values state.categories) state.locale)
      ]
  in {
    view = view,
    actions = [
      loadCategories router
    ]
  }

categoryHandler : Router Route State -> Handler State
categoryHandler router =
  let
    gallery' = gallery router
    navigation' = navigation router
    view state _ =
      let
        category = Dict.get "category" state.router.params &> flip Dict.get state.categories
        subcategory = Dict.get "subcategory" state.router.params &> flip Dict.get state.categories
        header = flip Maybe.map category <| \c -> innerHeader router state.locale (categoryLink router c state.locale True)

      in Dict.fromList <| List.filterMap identity [
        Maybe.map (\h -> ("header", h)) header
      , Just <| ("navigation", navigation' state.locale category subcategory)
      , Just <| ("body", gallery' state.router.params state.photos state.time)
      ]
  in {
    view = view,
    actions = [
      loadPhotos router
    , (\state -> mapDefault (getCategory state) (doNothing state) (\c -> setMetaFromCategory c state))
    ]
  }

photoHandler : Router Route State -> Handler State
photoHandler router =
  let
    view state _ =
      let
        pid = Maybe.map ((Result.withDefault 0) << String.toInt) <| Dict.get "photo" state.router.params
        params = Maybe.andThen pid <| \p ->
          let
            photo = state.photo
            keys = List.map .id state.photos
            neghbors = List.Extra.elemIndex p keys
              &> \idx ->
                let
                  l = List.length keys - 1
                  prev = if idx == 0 then List.Extra.last keys else keys !! (idx - 1)
                  next = if idx == l then List.head keys else keys !! (idx + 1)
                in Maybe.map2 (,) prev next
          in Maybe.map2 (\p n -> {photo = p, neighbors = n}) photo neghbors

        photo' = flip Maybe.map params <| \p -> photoWidget router state.router.params p.photo p.neighbors state.window state.locale
      in Dict.fromList <| mapDefault photo' [] <| \p -> [("photo", p)]
  in {
    view = view,
    actions = [
      loadPhoto
    ]
  }
