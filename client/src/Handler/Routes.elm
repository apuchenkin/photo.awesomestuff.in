module Handler.Routes where

import MultiwayTree exposing (Tree (..), Forest)

type Route = Locale | Home | NotFound | About | Contacts | Category | Photo

routes : Forest Route
routes = [
    Tree Locale [
      Tree NotFound [],
      Tree About [],
      Tree Contacts [],
      Tree Home [
        Tree Category [
          Tree Photo []
        ]
      ]
    ]
  ]
