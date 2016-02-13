module Handler.Default where

import Dict
import Lib.Helpers exposing (noFx, singleton)
import Handler.Actions exposing (..)
import Lib.Types exposing (Router (..), Handler, Response (..))

import Html exposing (div, text, Html, button)
import Html.Attributes exposing (href,class)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Handler.Routes as Route exposing (Route)
import Handler.Widgets exposing (..)

localeHandler : Router Route State -> Handler State
localeHandler router =
  let
    view address state parsed = parsed
  in
    {
      view = view,
      actions = [setLocale router]
    }

homeHandler : Router Route State -> Handler State
homeHandler router =
  let
    loader = Html.div [class "loader"] []
    view address state parsed =
      let
        -- _ = Debug.log "homeHandler" state
        -- params = Dict.filter (\k _ -> List.member k ["locale"]) state.router.params
        (Router r) = router
        home = homeLink router state.router.params
        categories c = Html.div [class "categories"] <| List.map (\c -> categoryLink router (snd c) state.router.params) <| Dict.toList c
        rest = case parsed of
          Nothing   -> [categories state.categories]
          Just html -> [html]
      in Just <| div [] <| case state.isLoading of
        True  -> home :: rest ++ [loader]
        False -> home :: rest
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
        category = getCategory state
        -- photoParams = Dict.filter (\k _ -> List.member k ["locale", "category"]) state.router.params
        -- TODO: tuple is not equal by reference
        photos =  (\r params photos -> (\l -> div [] <| List.map (\p -> photoLink r p ("ru", "patagonia", "los-glaciares")) l) photos) router state.router.params state.photos
        parsed' = div [] <| Maybe.withDefault [] <| Maybe.map singleton parsed

      in Maybe.map (\c -> div [] [text <| toString c, photos, parsed'] ) category
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
