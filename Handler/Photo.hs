module Handler.Photo where

import Import

getPhotoR :: PhotoId -> Handler Value
getPhotoR photoId = do
    photo <- runDB $ get photoId
    let mViews = fmap succ (fmap photoViews photo)
    runDB $ update photoId [PhotoViews  =. case mViews of {Nothing -> 0; Just views -> views}]
    returnJson photo

getPhotosR :: Handler Value
getPhotosR = do
    photos <- runDB $ selectList [] [] :: Handler [Entity Photo]
    returnJson $ photos
