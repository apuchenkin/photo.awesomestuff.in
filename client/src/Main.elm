import Lib.Router exposing (..)

import Task exposing (Task)
import Html exposing (Html)
import Effects exposing (Never)

import Handler.Default exposing (..)
import Handler.Actions exposing (State)
import Handler.Routes exposing (..)

result : Lib.Router.Result State
result = runRouter {
  init = [],
  routes = routes,
  handlerMap = mapHandler,
  inputs = []
  }

mapHandler : Route -> Handler State
mapHandler r = case r of
  Home               -> homeHandler
  Error              -> errorHandler
  Category category  -> categoryHandler category
  Admin Dashboard    -> adminHandler
  _                  -> forwardHandler

main : Signal Html
main = result.html

port tasks : Signal (Task Never ())
port tasks = result.tasks -- Signal.map (\_ -> ) main
