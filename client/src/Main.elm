import Task     exposing (Task)
import Html     exposing (Html)
import Effects  exposing (Never)
import Dict     exposing (Dict)

import Handler.Default exposing (..)
import Handler.Actions exposing (State)
import Handler.Routes exposing (..)
import Lib.Helpers exposing (..)

import Lib.Router exposing (..)
import Lib.Types  exposing (RouteConfig, Router, RouterResult, Response (..))

import Mouse

config : Route -> RouteConfig Route State
config route = case route of
  Home -> {
      url = "/",
      handler = homeHandler
    }
  NotFound -> {
      url = "/404",
      handler = notFoundHandler
    }
  Category -> {
      url = ":category[/:subcategory]",
      handler = categoryHandler
    }
  Photo -> {
      url = "/photo/:photo",
      handler = photoHandler
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
    inputs = [
      -- Signal.map (\p -> (\state -> Response <| noFx state)) Mouse.position
    ]
  }

result : RouterResult State
result = runRouter router

main : Signal Html
main = result.html

port tasks : Signal (Task Never ())
port tasks = result.tasks -- Signal.map (\_ -> ) main
