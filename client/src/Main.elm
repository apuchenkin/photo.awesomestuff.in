port module Main exposing (..)

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
import Router.Types  exposing (RouteConfig, Router, RouterConfig (..), Response (..), Constraint (..), RouteParams)

config : Route -> RouteConfig Route State
config route = case route of
  Route.Locale -> {
    segment = "[/:locale]"
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
  , window = {width = 0, height = 0}
  , transition = {
      transitionIn = False
    , transitionOut = False
    }
  }

type alias Flags =
  { locale: String
  , time: Time
  }

setFlags : Flags -> State
setFlags flags = { initialState |
    locale = Locale.fromString flags.locale
  , time = flags.time
  }

main : Program Flags
main = Router.dispatch
    setFlags
    <| RouterConfig {
      html5 = True
    , removeTrailingSlash = True
    , layout = layout
    , transition = onTransition
    , routes = routes
    , routeConfig = config
    , subscriptions = \_ -> Window.resizes setDims
    }

-- port tasks : Signal (Task Never ())
-- port tasks = result.tasks


-- port meta = Signal.map .meta result.state

-- port t2 : Signal (Task Never ())
-- port t2 = Signal.map (\s -> Effects.toTask mailbox.address <| Effects.tick (always s)) <| Signal.dropRepeats <| Signal.map (\state -> toString state.router.route) result.state
--
-- port rs : Signal (List String)
-- port rs = mailbox.signal
