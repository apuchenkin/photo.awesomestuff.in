import Task     exposing (Task)
import Html     exposing (Html)
import Effects  exposing (Never)
import Dict     exposing (Dict)

import Handler.Locale as Locale exposing (Locale)
import Handler.Default exposing (..)
import Handler.Actions exposing (State, Meta)
import Handler.Routes exposing (..)

import Router
import Router.Types  exposing (RouteConfig, Router, RouterResult, Response (..), Constraint (..))
import Router.Helpers exposing (..)

-- import Mouse

config : Route -> RouteConfig Route State
config route = case route of
  Locale -> {
      segment = "[/:locale]",
      constraints = Dict.fromList [("locale", Enum ["ru", "en"])],
      handler = localeHandler
    }
  Home -> {
      segment = "/",
      constraints = Dict.empty,
      handler = homeHandler
    }
  NotFound -> {
      segment = "404",
      constraints = Dict.empty,
      handler = notFoundHandler
    }
  Static page -> {
      segment = page,
      constraints = Dict.empty,
      handler = staticHandler page
    }
  Category -> {
      segment = ":category[/:subcategory]",
      constraints = Dict.empty,
      handler = categoryHandler
    }
  Photo -> {
      segment = "/photo/:photo",
      constraints = Dict.fromList [("photo", Int)],
      handler = photoHandler
    }

initialMeta : Meta
initialMeta = {
    title = "PHOTO.AWESOMESTUFF.IN",
    links = []
  }

initialState : State
initialState = {
    meta        = initialMeta,
    locale      = Locale.fallbackLocale,
    categories  = Dict.empty,
    photos      = [],
    photo       = Nothing,
    isLoading   = False,
    mouse       = (0,0),
    router      = Router.initialState
  }

port localePort : Signal String

router : Router Route State
router = Router.router {
    init      = initialState,
    useCache  = True,
    fallback  = (NotFound, Dict.empty),
    fallbackHtml  = Html.text "error",
    routes    = routes,
    config    = config,
    inits = [
      Signal.map (\locale state -> Response <| noFx {state | locale = Locale.fromString locale}) localePort
    ],
    inputs = []
  }

result : RouterResult State
result = Router.runRouter router

main : Signal Html
main = result.html

port tasks : Signal (Task Never ())
port tasks = result.tasks -- Signal.map (\_ -> ) main

port meta : Signal Meta
port meta = Signal.map .meta result.state
