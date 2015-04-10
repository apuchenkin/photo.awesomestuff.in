module Handler.Photo where

import Import
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import Data.Data (Data)
import Data.Aeson (encode, withObject)

getPhotoR :: PhotoId -> Handler Value
getPhotoR photoId = do
    photo <- runDB $ get photoId
    let mViews = fmap succ (fmap photoViews photo)
    runDB $ update photoId [PhotoViews  =. case mViews of {Nothing -> 0; Just views -> views}]
    returnJson photo

getPhotosR :: Handler Value
getPhotosR = do
    maybeCategoryName <- lookupGetParam "category"
    case maybeCategoryName of
        Nothing -> do
            photos <- runDB $ selectList [] [] :: Handler [Entity Photo]
            returnJson $ map toCollectionPhoto photos
        Just categoryName -> do
            photos <- runDB
               $ E.select
               $ E.from $ \(photo `E.InnerJoin` pc `E.InnerJoin` category) -> do
                    E.on $ category ^. CategoryId   E.==. pc ^. PhotoCategoryCategory
                    E.on $ photo    ^. PhotoId      E.==. pc ^. PhotoCategoryPhoto
                    E.where_ (category ^. CategoryName E.==. E.val categoryName)
                    return photo
            returnJson $ map toCollectionPhoto photos
    where
        toCollectionPhoto :: (Entity Photo) -> Value
        toCollectionPhoto (Entity id Photo {..}) = object[
                "id"        .= id,
                "src"       .= photoSrc,
                "thumb"     .= photoThumb,
                "width"     .= photoWidth,
                "height"    .= photoHeight,
                "views"     .= photoViews
            ]