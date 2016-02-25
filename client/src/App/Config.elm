module App.Config where

type alias Config = {
    staticEndpoint : String
  , apiEndpoint : String
  , gallery : { height : Int, width : Int }
  , hostname : String
  , secret : String
  , gutter : Int
  , brickWidth: Int
  , title : String
  }

config : Config
config = {
    title = "PHOTO.AWESOMESTUFF.IN"
  , hostname = "http://localhost:8080"
  , apiEndpoint = "/api/v1"
  , staticEndpoint = "http://photo.awesomestuff.in/"
  , secret = "secret"
  , gutter = 10
  , brickWidth = 100
  , gallery = {
      width   = 320,
      height  = 240
    }
  }
