module Lib.Router where

import Json.Decode
import Http
import Task
import String
import Array
import Html     exposing (Html, text, div)
import Effects  exposing (Effects, Never)
import History  exposing (path, setPath)
import RouteParser exposing (..)
import MultiwayTree exposing (..)

type Route = Home | Admin AdminRoute

matchers =
  [ static Home "/" ] ++ (mapMatchers Admin adminMatchers)

-- can be delegated to a component without knowdedge of global routing:

type AdminRoute = Dashboard | Users

adminMatchers =
  [ static Dashboard "/admin", static Users "/users" ]

routes = Tree (static Home "home") [Tree (static (Admin Dashboard) "admin") [], Tree (static (Admin Users) "users") []]

runRouter : Signal String -> Signal Html
runRouter path =
  let
    mapPath p =
      let
        -- singleton : action -> List action
        singleton action = [ action ]

        -- messages : Signal.Mailbox (List action)
        messages = Signal.mailbox []

        -- address : Signal.Address action
        address = Signal.forwardTo messages.address singleton

        r = snd <| List.foldl mapSegment ([routes], []) <| List.filter (not << String.isEmpty) <| String.split "/" p
        handlers = List.map mapHandler r

        updateStep action ((state, effects)) =
            let
                (state', effects') = toState <| action state
            in
                (state', Effects.batch [effects, effects'])

        update actions (state, _) = List.foldl updateStep (state, Effects.none) actions

        sList = List.foldl (\h acc -> h.inputs ++ acc) [] handlers
        inputs = Signal.mergeMany (messages.signal :: List.map (Signal.map singleton) sList)

        result = Signal.foldp update ([], Effects.none) inputs

        -- state = Signal.map fst result

      -- in List.foldr (\h mh -> Signal.map (h.view address) state) (Signal.constant Nothing) handlers
      in div [] [text "nothing"]

    mapSegment s (forest, acc) =
      let
        maybeRoute = match (List.indexedMap (\i v -> mapMatcher (\m -> (i,m)) (datum v)) forest) s
        forest' = Maybe.withDefault [] <| Maybe.andThen maybeRoute (\m -> Maybe.map children (Array.get (fst m) (Array.fromList forest)))
        acc' = acc ++ [Maybe.map snd maybeRoute]
      in (forest', acc')

  in Signal.map mapPath path

  -- case match matchers path of
mapHandler : Maybe Route -> Handler (List String)
mapHandler r = case r of
  Just Home               -> homeHandler
  Nothing                 -> homeHandler
  _                       -> homeHandler

type alias State    = (Html, Effects ())

homeHandler : Handler (List String)
homeHandler = {
    init = [],
    view = (\sa s -> div [] [text "homeHandler"]),
    inputs = [
      Signal.constant loadCategories
    ]
  }
  -- let
  --   tsk = Http.get Json.Decode.string ("http://photo.awesomestuff.in/api/v1/category")
  -- in
  --   (div [] [text "userHandler"], Effects.task <| Task.map (\_ -> ()) <| Task.toMaybe tsk)

-- userHandler : State
-- userHandler = (div [] [text "userHandler"], Effects.none)
--
-- dashboardHandler : State
-- dashboardHandler = (div [] [text "dashboardHandler"], Effects.none)
--
-- errorHandler : State
-- errorHandler = (div [] [text "errorHandler"], Effects.task (setPath "404"))

loadCategories : Action (List String)
loadCategories state =
  let
    tsk = Http.get Json.Decode.string ("http://photo.awesomestuff.in/api/v1/category")
    tsk' = Task.andThen (Task.toMaybe tsk) (\r -> Task.succeed (updateCategories <| Maybe.withDefault "sup" r))
  in DirtyState (state, Effects.task <| tsk')

updateCategories : String -> Action (List String)
updateCategories s state = DirtyState ([s], Effects.none)

type DirtyState state = DirtyState (state, Effects (Action state))

toState : DirtyState state -> (state, Effects (Action state))
toState (DirtyState s) = s

type alias Action state = state -> DirtyState state

type alias Handler state =
    { init    : state
    , view    : Signal.Address (Action state) -> state -> Html
    , inputs  : List (Signal.Signal (Action state))
    }
