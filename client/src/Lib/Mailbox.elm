module Lib.Mailbox where

import Lib.Helpers  exposing (singleton)
import Lib.Types    exposing (Action)

mailbox : Signal.Mailbox (List (Action state))
mailbox = Signal.mailbox []

address : Signal.Address (Action state)
address = Signal.forwardTo mailbox.address singleton
