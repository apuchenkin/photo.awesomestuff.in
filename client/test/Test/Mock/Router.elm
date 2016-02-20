module Test.Mock.Router where

import Dict
import Html             exposing (Html)

import Lib.Matcher
import Lib.Helpers      exposing (..)
import Lib.Types        exposing (..)

-- import Response as R
initialState : RouterState route
initialState = {
    route = Nothing
  , params = Dict.empty
  , cache = {unwrap = Dict.empty, rawUrl = Dict.empty, traverse = Dict.empty}
  }

bindForwardMock : RouterConfig route (WithRouter route state) -> Route route -> List Html.Attribute -> List Html.Attribute
bindForwardMock config route attrs = attrs

buildUrlMock : RouterConfig route (WithRouter route state) -> Route route -> String
buildUrlMock config (route, params) =
  let
    raw =  Lib.Matcher.composeRawUrl (.segment << config.config) config.routes route
    raws = Lib.Matcher.unwrap raw
  in Lib.Matcher.buildRawUrl raws (route, params)

forwardMock : RouterConfig route (WithRouter route state) -> Route route -> Action (WithRouter route state)
forwardMock config route state = Response <| noFx state

routerMock : RouterConfig route (WithRouter route state) -> Router route (WithRouter route state)
routerMock config = Router {
    config        = config
  , bindForward   = bindForwardMock   config
  , buildUrl      = buildUrlMock      config
  , forward       = forwardMock       config
  }
