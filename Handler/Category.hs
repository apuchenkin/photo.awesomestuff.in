module Handler.Category where

import Import
import Handler.Common                    (appendTranslations)
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import qualified Handler.Photo           as Photo

getCategoryR :: Handler Value
getCategoryR = do
  cacheSeconds $ 60 * 60 * 24 * 30 -- month
  addHeader "Vary" "Accept-Language, Authorization"
  maid    <- maybeAuthId
  categories  <- runDB
      $ E.select
      $ E.from $ \category -> do
        E.orderBy [E.desc (category ^. CategoryDate)]

        case maid of
          Nothing -> E.where_ (category ^. CategoryHidden E.==. E.val False)
          Just _  -> return ()

        return category

  results <- appendTranslations categories CategoryType

  returnJson results

getCategoryPhotoR :: CategoryId -> Handler Value
getCategoryPhotoR cid = do
  mcategory <-  runDB $ get cid
  categoryPhotos mcategory

getCategoryPhoto2R :: String -> Handler Value
getCategoryPhoto2R name = do
  mcategory <-  runDB $ getBy $ UniqueName name
  categoryPhotos $ entityVal <$> mcategory

categoryPhotos :: Maybe Category -> Handler Value
categoryPhotos mcategory = do
  cacheSeconds $ 24 * 60 * 60 -- day
  addHeader "Vary" "Accept-Language, Authorization"
  category  <- maybe notFound return mcategory
  maid      <- maybeAuthId
  request   <- getRequest
  when (categoryHidden category && isNothing maid) notFound

  let f = Photo.parseFilter request
      filterLens x = x { Photo.filterCategory = Just $ categoryName category }
      photoFilter = filterLens f

  photos  <- Photo.listPhotos photoFilter maid
  returnJson $ flip map photos $ case maid of
      Nothing -> Photo.toCollectionPhoto
      Just _  -> toJSON

linkCategoryPhotoR :: CategoryId -> Handler Value
linkCategoryPhotoR cid = do
  pids <- getRequestBody :: Handler [PhotoId]
  pcid <- runDB $ mapM (insertUnique . PhotoCategory cid) pids
  returnJson pcid

unlinkCategoryPhotoR :: CategoryId -> Handler Value
unlinkCategoryPhotoR cid = do
  pids <- getRequestBody :: Handler [PhotoId]
  pcid <- runDB $ mapM (deleteBy . UniquePhotoCategory cid) pids
  returnJson pcid
