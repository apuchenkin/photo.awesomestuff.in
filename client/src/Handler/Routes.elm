module Handler.Routes where

import MultiwayTree exposing (Tree (..), Forest)

type Route = Home | Error | Category | Photo

routes : Forest Route
routes = [
    Tree Home [
      Tree Category [
        Tree Photo []
      ]
    ],
    Tree Error []
  ]
