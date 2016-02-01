module Handler.Routes where

-- import MultiwayTree exposing (Tree (..), Forest)
-- import RouteParser exposing (..)

type Route = Home | Error | Category String | Photo String Int

-- routes : Forest Route
-- routes = [
--     Tree Home [
--       Tree (Category "") []
--       -- Tree (dyn1 Category "" string "") [],
--       -- Tree (static (Admin Dashboard) "admin") [],
--       -- Tree (static (Admin Users) "users") []
--     ],
--     Tree Error []
--   ]
