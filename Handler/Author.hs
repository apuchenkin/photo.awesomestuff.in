module Handler.Author where

import Import
import qualified Handler.Photo           as Photo

getAuthorPhotoR :: AuthorId -> Handler Value
getAuthorPhotoR aid = do
  cacheSeconds $ 24 * 60 * 60 -- day
  addHeader "Vary" "Accept-Language, Authorization"
  mauthor   <- runDB $ get aid
  _         <- maybe notFound return mauthor
  maid      <- maybeAuthId
  request   <- getRequest

  let f = Photo.parseFilter request
      filterLens x = x { Photo.filterAuthor = Just aid }
      photoFilter = filterLens f

  photos  <- Photo.listPhotos photoFilter maid
  returnJson $ flip map photos $ case maid of
      Nothing -> Photo.toCollectionPhoto
      Just _  -> toJSON