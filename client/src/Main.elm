import Task     exposing (Task)
import Html     exposing (Html)
import Effects  exposing (Never)
import Dict     exposing (Dict)
import Time     exposing (Time)
import Window

import App.Locale as Locale exposing (Locale)
import App.Model exposing (..)
import App.Actions exposing (..)
import App.Routes as Route exposing (Route, routes)
import App.Layout exposing (layout)
import Handler.Default exposing (..)

import Router
import Router.Types  exposing (RouteConfig, Router, RouterResult, RouterConfig (..), Response (..), Constraint (..), RouteParams)

config : Route -> RouteConfig State
config route = case route of
  Route.Locale -> {
    segment = "[/:locale]"
  , constraints = Dict.fromList [("locale", Enum ["ru", "en"])]
  , handler = localeHandler router
  }
  Route.Home -> {
    segment = ""
  , constraints = Dict.empty
  , handler = homeHandler router
  }
  Route.NotFound -> {
    segment = "/404"
  , constraints = Dict.empty
  , handler = notFoundHandler router
  }
  Route.Static page -> {
    segment = "/" ++ page
  , constraints = Dict.empty
  , handler = staticHandler page router
  }
  Route.Category -> {
    segment = "/:category[/:subcategory]"
  , constraints = Dict.empty
  , handler = categoryHandler router
  }
  Route.Photo -> {
    segment = "/photo/:photo"
  , constraints = Dict.fromList [("photo", Int)]
  , handler = photoHandler router
  }

initialMeta : Meta
initialMeta = {
    title = "PHOTO.AWESOMESTUFF.IN",
    description = Locale.i18n Locale.fallbackLocale "META.DESCRIPTION" [],
    links = []
  }

initialState : State
initialState = {
    router = Router.initialState
  , meta = initialMeta
  , locale = Locale.fallbackLocale
  , categories = Dict.empty
  , photos = []
  , photo = Nothing
  , isLoading = 0
  , time = 0
  , window = (0,0)
  }

router : Router Route State
router = Router.router <| RouterConfig {
    init = initialState
  , html5 = True
  , removeTrailingSlash = True
  , fallback = (Route.NotFound, Dict.empty)
  , layout = layout
  , onTransition = transition
  , routes = routes
  , routeConfig = config
  , inits = [
      Signal.map (setLocale << Locale.fromString) localePort
    , Signal.map setTime timePort
    , Signal.map setDims Window.dimensions
    ]
  , inputs = []
  }

result : RouterResult State
result = Router.runRouter router

main : Signal Html
main = result.html

port localePort : Signal String
port timePort: Signal Time

port tasks : Signal (Task Never ())
port tasks = result.tasks -- Signal.map (\_ -> ) main

port meta : Signal Meta
port meta = Signal.map .meta result.state

port rs : Signal ()
port rs = Signal.map (always ()) result.state
