import Html exposing (div, text, Html, button)
import Task exposing (Task)
import Effects exposing (Never, toTask)
import Lib.Component exposing (..)
import Lib.Router exposing (..)
import History
-- global routing:

type alias App =
    { response : Signal Html
    , tasks : Signal (Task.Task Never ())
    }

type Action1 = UPDATE

list1 : Component (List String) a
list1 =
  let
    init = (["as"], Effects.none)
  in {
      init   = init
    , update = (\a s -> (s, Effects.none))
    , view   = (\sa state -> div [] (List.map (text) state))
    , inputs = []
  }

btn1 : Component String a
btn1 = {
      init   = ("button", Effects.none)
    , update = (\a s -> (s, Effects.none))
    , view   = (\sa s -> (div [] [text s]))
    , inputs = []
  }

-- app : App
-- app =
--   let
--     messages = Signal.mailbox []
--     result = Signal.map router path
--   in
--     { response = Signal.map fst result
--     , tasks    = Signal.map (Effects.toTask messages.address << snd) result
--     }

main : Signal Html
main = runRouter History.path
  -- let
  --   result1 = dispatch list1
  --   result2 = dispatch btn1
  -- in Signal.merge result1.html result2.html

-- port tasks : Signal (Task Never ())
-- port tasks = app.tasks -- Signal.map (\_ -> ) main
