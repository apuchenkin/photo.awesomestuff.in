import Lib.Router exposing (..)
import MultiwayTree exposing (Tree (..))
import RouteParser exposing (..)

import Task exposing (Task)
import Html exposing (Html)
import Effects exposing (Never)

import Handler.Default exposing (..)
import Handler.Actions exposing (State)

result : Lib.Router.Result State
result = runRouter {
  init = [],
  routes = routes,
  handlerMap = mapHandler,
  inputs = []
  }


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

main : Signal Html
main = result.html

port tasks : Signal (Task Never ())
port tasks = result.tasks -- Signal.map (\_ -> ) main
