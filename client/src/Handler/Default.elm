module Handler.Default where

import String
import Dict
import Either
import Html exposing (div, text, Html, button)
import Html.Attributes exposing (href,class)
import List.Extra as List'
import Router.Types exposing (Router, Handler, Response (..))

import App.Routes as Route exposing (Route)
import App.Locale as Locale exposing (Locale)
import App.Actions exposing (..)
import Handler.Widgets exposing (..)

localeHandler : Router Route State -> Handler State
localeHandler router =
  let
    view address state _ = Dict.empty
  in
    {
      view = view,
      actions = [
        setLocale router,
        createLinks router
      ]
    }

staticHandler : String -> Router Route State -> Handler State
staticHandler page router =
  let
    body = div [] [text page]
    view address state parsed = Dict.fromList [
      ("body", body)
    ]
  in
    {
      view = view,
      actions = []
    }

childs : Category -> List Category -> List Category
childs category categories =
  let (Category pc) = category
  in flip List.filter categories <| \(Category c) ->
  Maybe.withDefault False <| c.parent &> \p -> Just <| Either.elim ((==) pc.id) ((==) category) p

homeHandler : Router Route State -> Handler State
homeHandler router =
  let
    view address state parsed =
      let
        -- _ = Debug.log "homeHandler" state.router.route
        category = Dict.get "category" state.router.params &> flip Dict.get state.categories

        header = case state.router.route of
          Just (Route.Static p) -> innerHeader router state.locale (Html.text p)
          _    -> case category of
            Just c  -> innerHeader router state.locale (categoryLink router c state.locale True)
            Nothing -> homeHeader router state.locale

        renderCategories categories = Html.div [class "galleries"] [
            Html.h2 [] [text <| Locale.i18n state.locale "Galleries" []],
            Html.ul []
                <| List.map (\c -> Html.li [] [categoryWidget router c (childs c categories) state.locale])
                <| List.filter (\(Category c) -> c.parent == Nothing) categories
              ]
        body = Html.div [class "content"] [renderCategories <| Dict.values state.categories]

      in Dict.fromList [
        ("header", header)
      , ("body", body)
      , ("footer", footer router state.locale)
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
    view address state parsed =
      let
        -- _ = Debug.log "categoryHandler" state
        category = Dict.get "category" state.router.params &> flip Dict.get state.categories
        subcategory = Dict.get "subcategory" state.router.params &> flip Dict.get state.categories

        navigation = Html.nav [class "categories"] [
          Html.ul []
            <| List.map    (\c -> Html.li [] [categoryLink router c state.locale (Just c == subcategory)])
            <| Maybe.withDefault []
            <| flip Maybe.map category <| \c -> childs c (Dict.values state.categories)
        ]

      in Dict.fromList [
        ("navigation", navigation)
      , ("body", gallery router state.router.params (Dict.values state.photos) state.time)
      ]
  in
    {
      view = view,
      actions = [
        loadPhotos router
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
            photo = Dict.get p state.photos
            keys = Dict.keys state.photos
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

        photo' = flip Maybe.map params <| \p -> photoWidget router state.router.params p.photo p.neighbors
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

notFoundHandler : Router Route State -> Handler State
notFoundHandler _ =
  let
    body locale = div [] [text <| Locale.i18n locale "404" []]
    view address state _ = Dict.fromList [
      ("body", body state.locale)
    ]
  in
    {
      view = view,
      actions = []
    }
