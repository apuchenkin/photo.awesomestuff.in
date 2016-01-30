import Html exposing (div, text, Html, button)
import Task exposing (Task)
import Effects exposing (Never)
import Lib.Router exposing (..)
import History exposing (setPath)
import MultiwayTree exposing (..)
import RouteParser exposing (..)

import Json.Decode
import Http
import Html.Events
import History

result : Lib.Router.Result State
result = runRouter {
  init = [],
  routes = routes,
  handlerMap = mapHandler,
  inputs = []
  }

type alias State = List String

type Route = Home | Error | Admin AdminRoute

type AdminRoute = Dashboard | Users

routes : Tree (Matcher Route)
routes = Tree (static Home "") [
  Tree (static Error "404") [],
  Tree (static (Admin Dashboard) "admin") [],
  Tree (static (Admin Users) "users") []
  ]

mapHandler : Route -> Handler State
mapHandler r = case r of
  Home               -> homeHandler
  Error              -> errorHandler
  Admin Dashboard    -> adminHandler
  _                  -> forwardHandler


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

forward : Action State
forward state =
  let
    tsk  = setPath "/404"
    tsk' = Task.andThen (Task.toMaybe tsk) (\r -> Task.succeed (\a -> DirtyState (a, Effects.none)))
  in DirtyState (state, Effects.task <| tsk')


loadCategories : Action State
loadCategories state =
  let
    tsk = Http.get Json.Decode.string ("http://photo.awesomestuff.in/api/v1/category")
    tsk' = Task.andThen (Task.toMaybe tsk) (\r -> Task.succeed (updateCategories <| Maybe.withDefault "supgf" r))
  in DirtyState (state, Effects.task <| tsk')

updateCategories : String -> Action State
updateCategories s state = DirtyState ([s], Effects.none)

main : Signal Html
main = result.html

port tasks : Signal (Task Never ())
port tasks = result.tasks -- Signal.map (\_ -> ) main
