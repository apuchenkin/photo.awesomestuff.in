import Lib.Router exposing (..)

import Task exposing (Task)
import Html exposing (Html)
import Effects exposing (Never)

import Handler.Default exposing (..)
import Handler.Actions exposing (State)
import Handler.Routes exposing (..)
import RouteParser as R exposing (static, dyn1)

config : Route -> RouteConfig Route State
config r = case r of
  Home               -> home
  Error              -> error404
  Category c         -> category c
  _                  -> error404

initialState : State
initialState = {
    categories = [],
    router = Lib.Router.initialState
  }

router : Router Route State
router = Lib.Router.router {
  init = initialState,
  routes = routes,
  config = config,
  inputs = []
  }

result : Lib.Router.Result State
result = runRouter router

home : RouteConfig Route State
home = {
    url = "",
    buildUrl = "",
    matcher = static Home "/",
    -- constructor = (\_ -> Home),
    -- params = [],
    handler = homeHandler router
  }

error404 : RouteConfig Route State
error404 = {
    url = "404",
    buildUrl = "404",
    matcher = static Error "404",
    handler = homeHandler router
  }

category : String -> RouteConfig Route State
category c = {
    url = "#category",
    buildUrl = c,
    matcher = dyn1 Category "" R.string "",
    handler = categoryHandler router c
  }

main : Signal Html
main = result.html

port tasks : Signal (Task Never ())
port tasks = result.tasks -- Signal.map (\_ -> ) main
