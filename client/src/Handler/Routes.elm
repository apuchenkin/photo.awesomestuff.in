module Handler.Routes where

import MultiwayTree exposing (Tree (..))
import RouteParser exposing (..)

type Route = Home | Error | Category String | Admin AdminRoute
type AdminRoute = Dashboard | Users

routes : Tree (Matcher Route)
routes = Tree (static Home "") [
    Tree (static Error "404") [],
    Tree (dyn1 Category "" string "") [],
    Tree (static (Admin Dashboard) "admin") [],
    Tree (static (Admin Users) "users") []
  ]
