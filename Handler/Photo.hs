module Handler.Photo where

import Import
import System.Directory

getPhotoR :: Handler Value
getPhotoR = error "Not yet implemented: getPhotoR"

getPhotosR :: Handler Value
getPhotosR = do
    photos <- runDB $ selectList [] [] :: Handler [Entity Photo]
    returnJson $ photos
