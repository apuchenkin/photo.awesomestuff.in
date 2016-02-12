import Task     exposing (Task)
import Html     exposing (Html)
import Effects  exposing (Never)
import Dict     exposing (Dict)

import Handler.Default exposing (..)
import Handler.Actions exposing (State)
import Handler.Routes exposing (..)
-- import Lib.Helpers exposing (..)

import Lib.Router exposing (..)
import Lib.Types  exposing (RouteConfig, Router, RouterResult, Response (..), Constraint (..))

-- import Mouse

config : Route -> RouteConfig Route State
config route = case route of
  Locale -> {
      url = "/[:locale]",
      constraints = Dict.fromList [("locale", Enum ["ru", "en"])],
      handler = localeHandler
    }
  NotFound -> {
      url = "/404",
      constraints = Dict.empty,
      handler = notFoundHandler
    }
  Home -> {
      url = "[/]",
      constraints = Dict.empty,
      handler = homeHandler
    }
  Category -> {
      url = ":category[/:subcategory]",
      constraints = Dict.empty,
      handler = categoryHandler
    }
  Photo -> {
      url = "/photo/:photo",
      constraints = Dict.fromList [("photo", Int)],
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
    fallback = (NotFound, Dict.empty),
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
