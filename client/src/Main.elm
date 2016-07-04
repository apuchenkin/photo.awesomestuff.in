port module Main exposing (..)

import Task
import Dict     exposing (Dict)
import Time     exposing (Time)
import Window
import AnimationFrame

import App.Locale as Locale exposing (Locale)
import App.Model exposing (..)
import App.Actions exposing (..)
import App.Routes as Route exposing (Route, routes)
import App.Layout exposing (layout)
import Handler.Default exposing (..)

import Router
import Router.Helpers exposing (performTask)
import Router.Types  exposing (RouteConfig, Router, RouterConfig (..), Response (..), Constraint (..), RouteParams, Action)

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
    title = "PHOTO.AWESOMESTUFF.IN"
  , description = Locale.i18n Locale.fallbackLocale <| Locale.Meta Locale.Description
  , links = []
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
  , defer = []
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

setFlags : Flags -> (State, Cmd (Action State))
setFlags flags = let
  state = { initialState |
    locale = Locale.fromString flags.locale
  , time = flags.time
  }
  in state ! [performTask <| Window.size `Task.andThen` (\size -> Task.succeed <| setSize size)]

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
    , subscriptions = \state -> Sub.batch [
        Window.resizes setSize
      , case state.defer of
        [] -> Sub.none
        _ -> AnimationFrame.times tick
      ]
    }
