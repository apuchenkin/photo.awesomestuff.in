module Handler.Default where

import Handler.Actions exposing (..)
import Lib.Router exposing (Handler, DirtyState (..), forward, bindForward)
import Html exposing (div, text, Html, button)
import Effects  exposing (Effects, Never)
import Html.Attributes exposing (href)
import Handler.Routes as Route
import Html.Events

homeHandler : Handler State
homeHandler =
  let
    home = Html.a [href "/"] [text "HOME"]
    view address state parsed = Just <| case parsed of
      Nothing   -> div [] [home, div [] <| List.map (\c -> Html.a ((bindForward (Route.Category c.name)) []) [text c.title]) state]
      Just html -> div [] [home, html]
  in
    {
      view = view,
      inputs = [
        loadCategories
      ]
    }

categoryHandler : String -> Handler State
categoryHandler category =
  let
    view address state parsed = Just <| div [] [text <| category]
  in
    {
      view = view,
      inputs = [
        (\state -> case List.filter (\c -> c.name == category) state of
          []         -> forward Route.Error state
          [caregory] -> DirtyState (state, Effects.none)
          _          -> forward Route.Error state
        )
      ]
    }

adminHandler : Handler State
adminHandler =
  let
    view address state parsed = Just <| div [] [Html.button [Html.Events.onClick address (updateCategories [])] [text "admin"]]
  in
    {
      view = view,
      inputs = []
    }

errorHandler : Handler State
errorHandler =
  let
    view address state parsed = Just <| div [] [text <| "404"]
  in
    {
      view = view,
      inputs = [
        -- forward
      ]
    }

forwardHandler : Handler State
forwardHandler =
  let
    view address state parsed = Nothing
  in
    {
      view = view,
      inputs = [
        forward Route.Error
      ]
    }
