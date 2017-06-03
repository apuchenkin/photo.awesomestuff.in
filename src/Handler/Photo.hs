{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Photo where

import Import
import Handler.Common                    (getTranslations, getLangs)
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import           Data.Aeson              (withObject, (.:?))
import qualified Data.HashMap.Strict     as H (union, insert, filterWithKey)
import           Database.Persist.Sql    (fromSqlKey)
import           Data.Text.Read          (decimal)
import           Data.Either.Combinators (rightToMaybe)

data FieldValue e = forall typ. PersistField typ => FieldValue { unField :: EntityField e typ, unValue :: typ}

data PhotoFilter = PhotoFilter {
    filterCategory  :: Maybe String,
    filterAuthor    :: Maybe AuthorId,
    filterHidden    :: Maybe Bool
} deriving Show

data PhotoPatch = PhotoPatch {
    patchName    :: Maybe Text,
    patchOrder   :: Maybe Int,
    patchHidden  :: Maybe Bool,
    patchGroup   :: Maybe (Maybe Int)
} deriving Show

instance FromJSON PhotoPatch where
    parseJSON = withObject "PhotoPatch" $ \o -> do
        patchName    <- o .:?  "name"
        patchOrder   <- o .:?  "order"
        patchHidden  <- o .:?  "hidden"
        patchGroup   <- o .:?  "group"

        return PhotoPatch {..}

photoDataReader :: PhotoPatch -> [FieldValue Photo]
photoDataReader PhotoPatch {..} = catMaybes [
    FieldValue PhotoName  . unpack <$> patchName,
    FieldValue PhotoOrder . Just   <$> patchOrder,
    FieldValue PhotoHidden         <$> patchHidden,
    FieldValue PhotoGroup          <$> patchGroup
  ];

toFilter :: PhotoPatch -> [Update Photo]
toFilter photoData = map buildFilter $ photoDataReader photoData where
    buildFilter :: FieldValue Photo -> Update Photo
    buildFilter FieldValue {unField = f, unValue = v} = f  =. v

-- request hadlers -----------------

getPhotoR :: PhotoId -> Handler Value
getPhotoR photoId = do
    cacheSeconds $ 24 * 60 * 60 -- day
    addHeader "Vary" "Accept-Language"
    langs   <- languages
    maid    <- maybeAuthId
    mphoto  <- runDB $ get photoId
    photo   <- maybe notFound return mphoto
    _       <- when (photoHidden photo && isNothing maid) notFound
    ma      <- maybe (return Nothing) (runDB . get) (photoAuthor photo)

    runDB $ update photoId [PhotoViews  =. succ (photoViews photo)]

    let entity = Entity photoId photo
    (Object translations) <- getTranslations entity PhotoType Nothing
    langs <- getLangs entity PhotoType ["caption"]

    let expose = ["id", "name", "src", "width", "height", "views", "datetime"]
        (Object r) = toJSON entity
        result = H.filterWithKey (\k _ -> elem k expose) r
        result' = H.union result translations
        result'' = H.insert "langs" langs result'
        result''' = H.insert "author" (toJSON ma) result''

    return $ Object result'''

patchPhotoR :: PhotoId -> Handler Value
patchPhotoR photoId = do
  photoData <- getRequestBody :: Handler PhotoPatch
  photo     <- runDB $ updateGet photoId (toFilter photoData)
  returnJson photo

----------------------------------------------------------------------
-- Service layer
----------------------------------------------------------------------

-- filters out fields from photo object

toCollectionPhoto :: Entity Photo -> Value
toCollectionPhoto (Entity pid Photo {..}) = object [
    "id"        .= pid,
    "src"       .= photoSrc,
    "width"     .= photoWidth,
    "height"    .= photoHeight,
    "views"     .= photoViews,
    "group"     .= photoGroup,
    "datetime"  .= photoDatetime
  ]

-- retrieve filter params from request
parseFilter :: YesodRequest -> PhotoFilter
parseFilter request = PhotoFilter category author hidden
  where
    query    = reqGetParams request
    category = unpack <$> lookup "category" query
    author   = E.toSqlKey . fst <$> join ((rightToMaybe . decimal) <$> lookup "author" query)
    hidden   = case lookup "hidden" query of
      Just "true"   -> Just True
      Just "false"  -> Just False
      _             -> Nothing

-- gets list of photos from database
listPhotos :: PhotoFilter -> Maybe UserId -> Handler [Entity Photo]
listPhotos PhotoFilter {..} auth = runDB
  $ E.select
  $ E.from $ \(  photo
  `E.InnerJoin`  pc
  `E.InnerJoin`  category ) -> do
    E.on $ category ^. CategoryId   E.==. pc     ^. PhotoCategoryCategory
    E.on $ photo    ^. PhotoId      E.==. pc     ^. PhotoCategoryPhoto
    E.orderBy [E.asc (photo ^. PhotoDatetime)]

    forM_ filterCategory $ \c -> E.where_ $ category ^. CategoryName E.==. E.val c
    forM_ filterAuthor   $ \a -> E.where_ $ photo    ^. PhotoAuthor  E.==. E.val (Just a)

    case auth of
      Nothing -> do
        E.where_ $ photo ^. PhotoHidden        E.==. E.val False
        E.where_ $ category ^. CategoryHidden  E.==. E.val False
      Just _  ->
        forM_ filterHidden  $ \hidden -> E.where_ $ photo ^. PhotoHidden E.==. E.val hidden

    return photo
