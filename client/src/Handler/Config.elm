module Handler.Config where

type alias Config = {
    staticEndpoint : String
  , apiEndpoint : String
  , gallery     : { height : Int, width : Int }
  , hostname    : String
  , secret      : String
  , title       : String
  }

config : Config
config = {
    title           = "PHOTO.AWESOMESTUFF.IN",
    hostname        = "http://localhost:3000",
    apiEndpoint     = "/api/v1",
    staticEndpoint  = "http://photo.awesomestuff.in/",
    secret          = "secret",
    gallery         = {
      width   = 320,
      height  = 240
    }
  }
