module Handler.Default where

import Dict
import Either
import Html exposing (div, text, Html, button)
import Html.Attributes exposing (href,class)
-- import Router.Helpers exposing (noFx, singleton)
import Router.Types exposing (Router (..), Handler, Response (..))

import Handler.Routes as Route exposing (Route)
import Handler.Widgets exposing (..)
import Handler.Locale as Locale exposing (Locale)
import Handler.Actions exposing (..)

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
    (Router r) = router
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
    (Router r) = router
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

        photos =  (\r params photos -> (\l -> div [] <| List.map (\p -> photoLink r p state.router.params) l) photos) router state.router.params state.photos

      in Dict.fromList [
        ("navigation", navigation)
      , ("body", photos)
      ]
      -- flip Maybe.map category <| \c -> div [] [
      --   navigation,
      --   photos,
      --   parsed'
      -- ]
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
        -- _ = Debug.log "photoHandler" state
        photo = Dict.get "photo" state.router.params
        photo' = div [] <| Maybe.withDefault [] <| flip Maybe.map photo <| \p -> [text <| toString p]
      in Dict.fromList [
        ("photo", photo')
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
