{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Handler.Photo where

import Import
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import qualified Network.Wai             as W
import           Data.Aeson              (Object, decode, eitherDecodeStrict, encode, withObject, (.:?))

data FieldValue e = forall typ. PersistField typ => FieldValue { unField :: EntityField e typ, unValue :: typ}

data PhotoData = PhotoData { 
    name    :: Maybe Text,
    order   :: Maybe Int,
    hidden  :: Maybe Bool
} deriving Show

instance FromJSON PhotoData where
    parseJSON = withObject "PhotoData" $ \o -> do
        name    <- o .:?  "name"
        order   <- o .:?  "order"
        hidden  <- o .:?  "hidden"

        return PhotoData {..}

photoDataReader :: PhotoData -> [FieldValue Photo]
photoDataReader PhotoData {..} = catMaybes [
        (\a -> FieldValue PhotoName (unpack a)) <$> name,
        (\a -> FieldValue PhotoOrder (Just a))  <$> order,
        (\a -> FieldValue PhotoHidden a)        <$> hidden
    ];

toFilter :: PhotoData -> [Update Photo]
toFilter photoData = map buildFilter $ photoDataReader photoData where
    buildFilter :: FieldValue Photo -> Update Photo
    buildFilter (FieldValue {unField = f, unValue = v}) = f  =. v

-- request hadlers -----------------

getPhotoR :: PhotoId -> Handler Value
getPhotoR photoId = do
    photo <- runDB $ get photoId
    let mViews = fmap succ (fmap photoViews photo)
    runDB $ update photoId [PhotoViews  =. case mViews of {Nothing -> 0; Just views -> views}]
    returnJson photo

patchPhotoR :: PhotoId -> Handler Value
patchPhotoR photoId = do
    request <- waiRequest
    body    <- liftIO $ W.requestBody request
    let maybeData = eitherDecodeStrict body
    case maybeData of 
        Left err -> invalidArgs [(pack err)]
        Right photoData -> do
            liftIO $ putStrLn $ pack $ show photoData
            runDB $ update photoId (toFilter photoData)
            photo     <- runDB $ get photoId
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