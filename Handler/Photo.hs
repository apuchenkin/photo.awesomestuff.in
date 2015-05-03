{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.Photo where

import Import
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

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
            returnJson ()
        Just categoryName -> do
            photos <- runDB
               $ E.select
               $ E.from $ \(author `E.InnerJoin` photo `E.InnerJoin` pc `E.InnerJoin` category ) -> do
                    E.on $ category ^. CategoryId              E.==. pc     ^. PhotoCategoryCategory
                    E.on $ photo    ^. PhotoId                 E.==. pc     ^. PhotoCategoryPhoto
                    E.on $ E.just (author   ^. AuthorId)       E.==. photo  ^. PhotoAuthor
                    E.orderBy [E.asc (photo ^. PhotoDatetime)]
                    E.where_ (category ^. CategoryName E.==. E.val (unpack categoryName))
                    return (photo, author)

            returnJson $ map toCollectionPhoto photos
    where
        toCollectionPhoto :: (Entity Photo, Entity Author) -> Value
        toCollectionPhoto (Entity pid Photo {..}, Entity _ author) = object [
                "id"        .= pid,
                "src"       .= photoSrc,
                "thumb"     .= photoThumb,
                "width"     .= photoWidth,
                "height"    .= photoHeight,
                "views"     .= photoViews,
                "author"    .= authorName author
            ]