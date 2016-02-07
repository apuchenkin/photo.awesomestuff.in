import Task     exposing (Task)
import Html     exposing (Html)
import Effects  exposing (Never)
import Dict     exposing (Dict)

import Handler.Default exposing (..)
import Handler.Actions exposing (State)
import Handler.Routes exposing (..)

import Lib.Router exposing (..)
import Lib.Types  exposing (RouteConfig, Router, RouterResult)

config : Route -> RouteConfig State
config route = case route of
  Home -> {
      url = "/",
      handler = homeHandler router
    }
  NotFound -> {
      url = "/404",
      handler = notFoundHandler router
    }
  Category -> {
      url = ":category[/:subcategory]",
      handler = categoryHandler router
    }
  Photo -> {
      url = "/:photo",
      handler = photoHandler router
    }

initialState : State
initialState = {
    categories = Dict.empty,
    photos = [],
    photo = Nothing,
    isLoading = False,
    router = Lib.Router.initialState
  }

router : Router Route State
router = Lib.Router.router {
    init = initialState,
    routes = routes,
    config = config,
    inputs = []
  }

result : RouterResult State
result = runRouter router

main : Signal Html
main = result.html

port tasks : Signal (Task Never ())
port tasks = result.tasks -- Signal.map (\_ -> ) main
