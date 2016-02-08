module Handler.Routes where

import MultiwayTree exposing (Tree (..), Forest)

type Route = Home | NotFound | Category | Photo

routes : Forest Route
routes = [
    Tree NotFound [],
    Tree Home [
      Tree Category [
        Tree Photo []
      ]
    ]
  ]
