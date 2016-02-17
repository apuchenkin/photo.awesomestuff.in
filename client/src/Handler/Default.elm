module Handler.Default where

import Dict
import Either
import Lib.Helpers exposing (noFx, singleton)
import Handler.Actions exposing (..)
import Lib.Types exposing (Router (..), Handler, Response (..))

import Html exposing (div, text, Html, button)
import Html.Attributes exposing (href,class)
import Handler.Routes as Route exposing (Route)
import Handler.Widgets exposing (..)

localeHandler : Router Route State -> Handler State
localeHandler router =
  let
    view address state parsed = parsed
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
    view address state parsed = Just <| div [] [
        text page
      ]
  in
    {
      view = view,
      actions = []
    }

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
            Just c  -> innerHeader router state.locale (categoryLink router c state.locale)
            Nothing -> homeHeader router state.locale

        categories c = Html.div [class "categories"] <| List.map (\c -> categoryLink router (snd c) state.locale) <| Dict.toList c
        body = case parsed of
          Nothing   -> categories state.categories
          Just v    -> v
      in Just <| div [] [
        loader state.isLoading,
        languageSelector router state,
        header,
        body,
        footer router state.locale
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
        -- category = getCategory state
        category = Dict.get "category" state.router.params &> flip Dict.get state.categories
        subcategory = Dict.get "subcategory" state.router.params &> flip Dict.get state.categories

        -- TODO: tuple is not equal by reference
        navigation = Html.nav [class "categories"] [
          Html.ul []
            <| List.map (\c -> Html.li [] [categoryLink router (snd c) state.locale])
            <| List.filter (\(_, Category pc) -> Maybe.withDefault False <| pc.parent &> \ p -> flip Maybe.map category (\(Category c) -> Either.elim ((==) (c.id)) ((==) (Category c)) p) )
            <| Dict.toList state.categories
        ]

        photos =  (\r params photos -> (\l -> div [] <| List.map (\p -> photoLink r p state.router.params) l) photos) router state.router.params state.photos
        parsed' = div [] <| Maybe.withDefault [] <| Maybe.map singleton parsed

        -- navigation = Maybe.map (\c -> Html.nav div [class "navigation"] [, ] ) category
      in flip Maybe.map category <| \c -> div [] [
        -- text <| toString c,
        navigation,
        photos, parsed'
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
        -- _ = Debug.log "photoHandler" state
        photo = Dict.get "photo" state.router.params
      in Maybe.map (\p -> div [] [text <| toString p] ) photo
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
    view address state _ = Just <| div [] [text <| "404"]
  in
    {
      view = view,
      actions = []
    }
