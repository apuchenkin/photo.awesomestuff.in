module Handler.Routes where

import MultiwayTree exposing (Tree (..))
-- import RouteParser exposing (..)

type Route = Home | Error | Category String | Admin AdminRoute
type AdminRoute = Dashboard | Users

routes : Tree Route
routes = Tree Home [
    Tree Error [],
    Tree (Category "") []
    -- Tree (dyn1 Category "" string "") [],
    -- Tree (static (Admin Dashboard) "admin") [],
    -- Tree (static (Admin Users) "users") []
  ]
