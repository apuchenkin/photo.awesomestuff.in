import Lib.Router exposing (..)

import Task exposing (Task)
import Html exposing (Html)
import Effects exposing (Never)

import Handler.Default exposing (..)
import Handler.Actions exposing (State)
import Handler.Routes exposing (..)

config : Route -> RouteConfig State
config r = case r of
  Home               -> home
  Error              -> error404
  Category c         -> category c
  _                  -> error404

router : Router State Route
router = Lib.Router.router {
  init = [],
  routes = routes,
  config = config,
  inputs = []
  }

result : Lib.Router.Result State
result = runRouter router

home : RouteConfig State
home = {
    url = "",
    params = [],
    handler = homeHandler router
  }

error404 : RouteConfig State
error404 = {
    url = "404",
    params = [],
    handler = homeHandler router
  }

category : String -> RouteConfig State
category c = {
    url = "#category",
    -- params = [("category", c)],
    params = [],
    handler = categoryHandler router c
  }

main : Signal Html
main = result.html

port tasks : Signal (Task Never ())
port tasks = result.tasks -- Signal.map (\_ -> ) main
