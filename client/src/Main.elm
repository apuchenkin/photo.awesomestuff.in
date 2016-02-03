import Lib.Router exposing (..)

import Task exposing (Task)
import Html exposing (Html)
import Effects exposing (Never)

import Handler.Default exposing (..)
import Handler.Actions exposing (State)
import Handler.Routes exposing (..)

config : Route -> RouteConfig State
config route = case route of
  Home               -> home
  Error              -> error404
  Category           -> category
  _                  -> error404

initialState : State
initialState = {
    categories = [],
    isLoading = False,
    router = Lib.Router.initialState
  }

home : RouteConfig State
home = {
    -- parent = Nothing,
    url = "/",
    -- buildUrl = "/",
    -- matcher = static Home "/",
    handler = homeHandler router
  }

error404 : RouteConfig State
error404 = {
    -- parent = Nothing,
    url = "/404",
    -- buildUrl = "/404",
    -- matcher = static Error "/404",
    handler = homeHandler router
  }

category : RouteConfig State
category = {
    -- parent = Just Home,
    url = "/:category",
    -- buildUrl = "/" ++ c,
    -- matcher = dyn1 Category "/" R.string "",
    handler = categoryHandler router
  }

-- photo : String -> Int -> RouteConfig Route State
-- photo category pid = {
--     parent = Just (Category category),
--     url = "/photo/:id",
--     buildUrl = "/" ++ category ++ "/photo/" ++ toString pid,
--     matcher = R.dyn2 Photo "/" R.string "/photo/" R.int "",
--     handler = [homeHandler router, categoryHandler router category]
--   }

router : Router Route State
router = Lib.Router.router {
    init = initialState,
    routes = routes,
    config = config,
    inputs = []
  }

result : Lib.Router.Result State
result = runRouter router

main : Signal Html
main = result.html

port tasks : Signal (Task Never ())
port tasks = result.tasks -- Signal.map (\_ -> ) main
