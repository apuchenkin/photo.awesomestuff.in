{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Handler.Photo where

import Import
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import qualified Network.Wai             as W
import           Data.Aeson              (eitherDecodeStrict, withObject, (.:?))

data FieldValue e = forall typ. PersistField typ => FieldValue { unField :: EntityField e typ, unValue :: typ}

data PhotoData = PhotoData {
    name    :: Maybe Text,
    order   :: Maybe Int,
    hidden  :: Maybe Bool,
    group   :: Maybe Int
} deriving Show

instance FromJSON PhotoData where
    parseJSON = withObject "PhotoData" $ \o -> do
        name    <- o .:?  "name"
        order   <- o .:?  "order"
        hidden  <- o .:?  "hidden"
        group   <- o .:?  "group"

        return PhotoData {..}

photoDataReader :: PhotoData -> [FieldValue Photo]
photoDataReader PhotoData {..} = catMaybes [
    FieldValue PhotoName  . unpack <$> name,
    FieldValue PhotoOrder . Just   <$> order,
    FieldValue PhotoHidden         <$> hidden,
    FieldValue PhotoGroup . Just   <$> group
  ];

toFilter :: PhotoData -> [Update Photo]
toFilter photoData = map buildFilter $ photoDataReader photoData where
    buildFilter :: FieldValue Photo -> Update Photo
    buildFilter (FieldValue {unField = f, unValue = v}) = f  =. v

-- request hadlers -----------------

getPhotoR :: PhotoId -> Handler Value
getPhotoR photoId = do
    photo <- runDB $ get photoId
    let mViews = fmap (succ . photoViews) photo
    runDB $ update photoId [PhotoViews  =. fromMaybe 0 mViews]
    returnJson photo

patchPhotoR :: PhotoId -> Handler Value
patchPhotoR photoId = do
    request <- waiRequest
    body    <- liftIO $ W.requestBody request
    case eitherDecodeStrict body of
        Left  err -> invalidArgs [pack err]
        Right photoData -> do
            runDB $ update photoId (toFilter photoData)
            photo     <- runDB $ get photoId
            returnJson photo

getPhotosR :: Handler Value
getPhotosR = do
  maid <- maybeAuthId
  maybeCategoryName <- lookupGetParam "category"
  case maybeCategoryName of
    Nothing -> returnJson ()
    Just categoryName -> do
      photos <- runDB
       $ E.select
       $ E.from $ \(author `E.InnerJoin` photo `E.InnerJoin` pc `E.InnerJoin` category ) -> do
          E.on $ category ^. CategoryId              E.==. pc     ^. PhotoCategoryCategory
          E.on $ photo    ^. PhotoId                 E.==. pc     ^. PhotoCategoryPhoto
          E.on $ E.just (author   ^. AuthorId)       E.==. photo  ^. PhotoAuthor
          E.orderBy [E.asc (photo ^. PhotoDatetime)]
          E.where_ (category ^. CategoryName E.==. E.val (unpack categoryName))
          case maid of
            Nothing -> E.where_ (photo ^. PhotoHidden E.==. E.val False)
            Just _  -> return ()

          return (photo, author)

      returnJson $ map (case maid of
        Nothing -> toCollectionPhoto
        Just _  -> toJSON . fst
        ) photos
  where
    toCollectionPhoto :: (Entity Photo, Entity Author) -> Value
    toCollectionPhoto (Entity pid Photo {..}, Entity _ author) = object [
        "id"        .= pid,
        "src"       .= photoSrc,
        "thumb"     .= photoThumb,
        "width"     .= photoWidth,
        "height"    .= photoHeight,
        "views"     .= photoViews,
        "group"     .= photoGroup,
        "author"    .= authorName author
      ]
