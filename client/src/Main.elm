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

config : Route -> RouteConfig Route State
config route = case route of
  Route.Locale -> {
    segment = "/[:locale]"
  , bypass = True
  , parent = Nothing
  , constraints = Dict.fromList [("locale", Enum ["ru", "en"])]
  , handler = localeHandler
  }
  Route.NotFound -> {
    segment = "/404"
  , bypass = False
  , parent = Just Route.Locale
  , constraints = Dict.empty
  , handler = notFoundHandler
  }
  Route.Static page -> {
    segment = "/" ++ page
  , bypass = False
  , parent = Just Route.Locale
  , constraints = Dict.empty
  , handler = staticHandler page
  }
  Route.Home -> {
    segment = ""
  , bypass = False
  , parent = Just Route.Locale
  , constraints = Dict.empty
  , handler = homeHandler
  }
  Route.Category -> {
    segment = "/:category[/:subcategory]"
  , bypass = False
  , parent = Just Route.Home
  , constraints = Dict.empty
  , handler = categoryHandler
  }
  Route.Photo -> {
    segment = "/photo/:photo"
  , bypass = False
  , parent = Just Route.Category
  , constraints = Dict.fromList [("photo", Int)]
  , handler = photoHandler
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
  , isLoading = False
  , time = 0
  , window = (0,0)
  }

result : RouterResult State
result = Router.runRouter <| RouterConfig {
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

main : Signal Html
main = result.html

port localePort : Signal String
port timePort: Signal Time

port tasks : Signal (Task Never ())
port tasks = result.tasks

port meta : Signal Meta
port meta = Signal.map .meta result.state

mailbox : Signal.Mailbox (List String)
mailbox = Signal.mailbox []

port t2 : Signal (Task Never ())
port t2 = Signal.map (\s -> Effects.toTask mailbox.address <| Effects.tick (always s)) <| Signal.dropRepeats <| Signal.map (\state -> toString state.router.route) result.state

port rs : Signal (List String)
port rs = mailbox.signal
