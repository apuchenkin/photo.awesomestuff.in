module Lib.Types (ActionEffects, Response (..), Action, Handler, RouteConfig, RouterResult, RouteParams, RouterState, Route,
                  GetRouteConfig, WithRouter, RouterConfig, Router (..), Transition, Constraint (..)) where

import Dict           exposing (Dict)
import Html           exposing (Html)
import Task           exposing (Task)
import Effects        exposing (Effects, Never)
import MultiwayTree   exposing (Tree, Forest)

-----------------------------------------
-- State
-----------------------------------------

type alias ActionEffects state = Effects (Action state)

type Response state = Response (state, ActionEffects state)

type alias Action state = state -> Response state

type alias Handler state = {
    view    : Signal.Address (Action state) -> state -> Maybe Html -> Maybe Html
  , inputs  : List (Action state)
  }

type Constraint = Int | String | Enum (List String) | Regex String

type alias RouteConfig route state = {
      url:          String
    , constraints:  Dict String Constraint
    , handler:      Router route state -> Handler state
  }

type alias RouterResult state =
    { html  : Signal Html
    , state : Signal state
    , tasks : Signal (Task Never ())
    }

type alias RouteParams  = Dict String String

type alias Route route = (route, RouteParams)

type alias RouterState route = {
    route: Maybe route,
    params: RouteParams,
    cache: {
      treeUrl: Dict String String,
      unwrap: Dict String (List String)
    }
  }

-----------------------------------------
-- Route
-----------------------------------------

type alias GetRouteConfig route state = route -> RouteConfig route state

{-| Type extension for the model. -}
type alias WithRouter route state = { state | router : RouterState route }

type alias RouterConfig route state = {
  init:       state,
  fallback:   Route route,
  config:     GetRouteConfig route state,
  routes:     Forest route,
  inits:      List (Signal.Signal (Action state)),
  inputs:     List (Signal.Signal (Action state))
}

type Router route state = Router {
  config        : RouterConfig route state,
  getRoute      : Maybe route,
  getParams     : RouteParams,
  bindForward   : Route route -> List Html.Attribute -> List Html.Attribute,
  buildUrl      : Route route -> String,
  forward       : Route route -> Action state
}

type alias Transition route state = Maybe route -> route -> Action state

--------------------------------------------------------------------------------
