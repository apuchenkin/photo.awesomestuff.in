module Handler.Routes where

import MultiwayTree exposing (Tree (..), Forest)

type Route = Locale | Home | NotFound | Static String | Category | Photo

routes : Forest Route
routes = [
    Tree Locale [
      Tree NotFound [],
      Tree (Static "about") [],
      Tree (Static "contacts") [],
      Tree Home [
        Tree Category [
          Tree Photo []
        ]
      ]
    ]
  ]
