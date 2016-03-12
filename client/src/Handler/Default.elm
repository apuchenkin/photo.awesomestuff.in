module Handler.Default where

import String
import Dict
import Html exposing (div, text, Html, button)
import Html.Attributes as Attr exposing (href,class)
import List.Extra as List'
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
    view address state _ = Dict.empty
  in
    {
      view = view,
      actions = [
        resolveLocale router,
        createLinks router
      ]
    }

notFoundHandler : Router Route State -> Handler State
notFoundHandler router =
  let
    view address state _ = Dict.fromList [
      ("body", notFoundWidget router state.locale)
    ]
  in
    {
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
    view address state _ = Dict.fromList [
      ("header", innerHeader router state.locale (Html.text <| title state.locale))
    , ("body", body state.locale)
    ]
  in
    {
      view = view,
      actions = [
        \state -> setTitle (Just <| title state.locale) state
      ]
    }

homeHandler : Router Route State -> Handler State
homeHandler router =
  let
    view address state _ = Dict.fromList [
        ("body", galleriesWidget router (Dict.values state.categories) state.locale)
      ]
  in
    {
      view = view,
      actions = [
        loadCategories router
      ]
    }

categoryHandler : Router Route State -> Handler State
categoryHandler router =
  let
    view address state _ =
      let
        -- _ = Debug.log "categoryHandler" state
        category = Dict.get "category" state.router.params &> flip Dict.get state.categories
        subcategory = Dict.get "subcategory" state.router.params &> flip Dict.get state.categories
        header = flip Maybe.map category <| \c -> innerHeader router state.locale (categoryLink router c state.locale True)

        navigation = Html.nav [class "categories"] [
          Html.ul []
            <| List.map    (\c -> Html.li [] [categoryLink router c state.locale (Just c == subcategory)])
            <| Maybe.withDefault []
            <| flip Maybe.map category <| \c -> childs c (Dict.values state.categories)
        ]

      in Dict.fromList <| List.filterMap identity [
        Maybe.map (\h -> ("header", h)) header
      , Just <| ("navigation", navigation)
      , Just <| ("body", gallery router state.router.params state.photos state.time)
      ]
  in
    {
      view = view,
      actions = [
        loadPhotos router
      , (\state -> mapDefault (getCategory state) (doNothing state) (\c -> setMetaFromCategory c state))
      ]
    }

photoHandler : Router Route State -> Handler State
photoHandler router =
  let
    view address state _ =
      let
        pid = Maybe.map ((Result.withDefault 0) << String.toInt) <| Dict.get "photo" state.router.params
        params = Maybe.andThen pid <| \p ->
          let
            photo = state.photo
            keys = List.map .id state.photos
            neghbors = List'.elemIndex p keys
              &> \idx ->
                let
                  l = List.length keys - 1
                  last = List'.last keys
                  first = List.head keys
                  prev = if idx == 0 then last else List'.getAt keys (idx - 1)
                  next = if idx == l then first else List'.getAt keys (idx + 1)
                in Maybe.map2 (,) prev next
          in Maybe.map2 (\p n -> {photo = p, neighbors = n}) photo neghbors

        photo' = flip Maybe.map params <| \p -> photoWidget router address state.router.params p.photo p.neighbors state.window (isLoading state) state.locale
      in Dict.fromList <| List.filterMap identity [
        flip Maybe.map photo' <| \p -> ("photo", p)
      ]
  in
    {
      view = view,
      actions = [
        loadPhoto
      ]
    }
