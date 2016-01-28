import Html exposing (div, text, Html)
import Html.Attributes exposing (class)
import RouteParser exposing (..)
import History exposing (path, setPath)
import Task exposing (Task)
import Effects exposing (Never, toTask)

-- global routing:

type Route = Home | Admin AdminRoute

matchers =
  [ static Home "/" ] ++ (mapMatchers Admin adminMatchers)

-- can be delegated to a component without knowdedge of global routing:

type AdminRoute = Dashboard | Users

adminMatchers =
  [ static Dashboard "/admin", static Users "/users" ]

type Handler = Html
type alias State = (Html, Effects.Effects ())

homeHandler : State
homeHandler = (div [] [text "homeHandler"], Effects.none)

userHandler : State
userHandler = (div [] [text "userHandler"], Effects.none)

dashboardHandler : State
dashboardHandler = (div [] [text "dashboardHandler"], Effects.none)

errorHandler : State
errorHandler = (div [] [text "errorHandler"], Effects.task (setPath "404"))

router : String -> State
router path = case match matchers path of
  Just Home               -> homeHandler
  Just (Admin Dashboard)  -> dashboardHandler
  Just (Admin Users)      -> userHandler
  Nothing                 -> errorHandler

type alias App =
    { response : Signal Html
    , tasks : Signal (Task.Task Never ())
    }


-- app : Signal String -> Signal Html
-- app sig =
--   let
--     messages = Signal.mailbox []
--   in
--     Signal.map router sig
-- dispatch

app : App
app =
  let
    messages = Signal.mailbox []
    result = Signal.map router path
  in
    { response = Signal.map fst result
    , tasks    = Signal.map (Effects.toTask messages.address << snd) result
    }


main : Signal Html
main = app.response

port tasks : Signal (Task Never ())
port tasks = app.tasks -- Signal.map (\_ -> ) main
