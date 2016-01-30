module Handler.Default where

import Handler.Actions exposing (..)
import Lib.Router exposing (Handler)
import Html exposing (div, text, Html, button)
import Html.Events

homeHandler : Handler State
homeHandler =
  let
    view address state parsed = div [] [text <| "homeHandler: " ++ toString state, parsed]
  in
    {
      view = view,
      inputs = [
        loadCategories
      ]
    }

adminHandler : Handler State
adminHandler =
  let
    view address state parsed = div [] [Html.button [Html.Events.onClick address (updateCategories "ad")] [text "admin"]]
  in
    {
      view = view,
      inputs = []
    }

errorHandler : Handler State
errorHandler =
  let
    view address state parsed = div [] [text <| "404"]
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
    view address state parsed = div [] [text <| "404"]
  in
    {
      view = view,
      inputs = [
        forward
      ]
    }
