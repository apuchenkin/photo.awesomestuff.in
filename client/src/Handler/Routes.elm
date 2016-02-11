module Handler.Routes where

import MultiwayTree exposing (Tree (..), Forest)

type Route = Locale | Home | NotFound | Category | Photo

routes : Forest Route
routes = [
    Tree Locale [
      Tree NotFound [],
      Tree Home [
        Tree Category [
          Tree Photo []
        ]
      ]
    ]
  ]
