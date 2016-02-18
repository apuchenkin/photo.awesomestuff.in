module Lib.Router (router, runRouter, initialState) where

import Dict
import History
import Effects          exposing (Effects)
import Signal.Extra     exposing (fairMerge, foldp')

import Lib.Helpers      exposing (..)
import Lib.Types        exposing (..)
import Lib.Functions    exposing (..)
import Lib.Mailbox      exposing (..)

-- import Response as R
initialState : RouterState route
initialState = {
    route = Nothing
  , params = Dict.empty
  , cache = {unwrap = Dict.empty, rawUrl = Dict.empty, traverse = Dict.empty}
  }

{-| Router constructor -}
router : RouterConfig route (WithRouter route state) -> Router route (WithRouter route state)
router config =
  let
    state = if config.useCache
      then prepareCache config.init config
      else config.init

  in Router {
    config        = {config | init = state}
  , bindForward   = bindForward   config state.router.cache
  , buildUrl      = buildUrl      config state.router.cache
  , forward       = forward       config
  }

runRouter : Router route (WithRouter route state) -> RouterResult (WithRouter route state)
runRouter router =
  let
    -- _ = Debug.log "initialState" initialState
    (Router r) = router
    initialState = r.config.init
    init = (Signal.map (singleton << (,) True << setUrl router initialState.router) History.path)

    -- inputs : Signal (List (Bool, Action state))
    inputs =
      List.foldl (Signal.Extra.fairMerge List.append)
      init <|
      (Signal.map (List.map ((,) False)) mailbox.signal) -- actions from events
      :: List.map (Signal.map (singleton << (,) True))  r.config.inits
      ++ List.map (Signal.map (singleton << (,) False)) r.config.inputs

    -- update : List (Bool, Action state) -> (state, ActionEffects state) -> (state, ActionEffects state)
    update  actions (state,_) = List.foldl runAction (noFx state)
      <| List.map snd actions

    -- update' : List (Bool, Action state) -> (state, ActionEffects state)
    update' actions           = List.foldl runAction (noFx initialState)
      <| List.map snd
      <| List.filter fst actions

    result = foldp' update update' inputs
    state = Signal.map fst result
  in
    {
      html  = Signal.map (render router) state
    , state = state
    , tasks = Signal.map (Effects.toTask mailbox.address << snd) result
    }
