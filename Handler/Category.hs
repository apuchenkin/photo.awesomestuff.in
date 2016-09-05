module Handler.Category where

import Import
import Handler.Common                    (appendTranslations)
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import Database.Esqueleto.Internal.Sql   (veryUnsafeCoerceSqlExprValue)
import qualified Handler.Photo           as Photo
import qualified Data.HashMap.Strict     as H (insert, union, filterWithKey)

coerce :: E.SqlExpr (E.Value Int64) -> E.SqlExpr (E.Value (Key Category))
coerce = veryUnsafeCoerceSqlExprValue

getCategoriesR :: Handler Value
getCategoriesR = do
  cacheSeconds $ 60 * 60 * 24 * 30 -- month
  addHeader "Vary" "Accept-Language, Authorization"
  maid <- maybeAuthId
  langs <- languages
  categories  <- runDB
      $ E.select
      $ E.from $ \(category `E.InnerJoin` translation) -> do
        E.on $ category ^. CategoryId E.==. coerce( translation ^. TranslationRefId )
        E.where_ $ translation ^. TranslationLanguage E.==. E.val (pickLanguadge langs)
        E.where_ $ translation ^. TranslationRefType  E.==. E.val CategoryType
        E.where_ $ translation ^. TranslationField    E.==. E.val "title"
        E.orderBy [E.desc (category ^. CategoryDate)]

        case maid of
          Nothing -> E.where_ (category ^. CategoryHidden E.==. E.val False)
          Just _  -> return ()

        return (category, translation ^. TranslationValue)

  returnJson $ map (updateCategories maid) categories
  where
    expose = ["id", "name", "date", "parent", "image"]
    updateCategories :: Maybe UserId -> (Entity Category, E.Value Text) -> Value
    updateCategories maid (category, title) = Object hmap'
      where
          (Object c) = toJSON category
          hmap = case maid of
            Nothing -> H.filterWithKey (\k _ -> elem k expose) c
            Just _ -> c
          hmap' = H.insert "title" (toJSON $ E.unValue title) hmap

getCategoryR :: String -> Handler Value
getCategoryR name = do
  cacheSeconds $ 60 * 60 * 24 * 30 -- month
  addHeader "Vary" "Accept-Language"
  mcategory <-  runDB $ getBy $ UniqueName name
  (Entity cid category) <- maybe notFound return mcategory
  maid <- maybeAuthId
  langs <- languages
  _ <- when (categoryHidden category && isNothing maid) notFound

  mt <- runDB $ getBy $ UniqueTranslation
    (pickLanguadge langs)
    CategoryType
    (E.fromSqlKey cid)
    "title"

  mc <- runDB $ getBy $ UniqueTranslation
    (pickLanguadge langs)
    CategoryType
    (E.fromSqlKey cid)
    "description"

  translations <- runDB $ selectList [
      TranslationRefId ==. E.fromSqlKey cid,
      TranslationRefType ==. CategoryType,
      TranslationField ==. "title"
    ] []

  let expose = ["id", "name", "date", "parent", "image"]
      (Object r) = toJSON $ Entity cid category
      (Object e) = object [
          "title" .= fmap (translationValue . entityVal) mt,
          "description" .= fmap (translationValue . entityVal) mc,
          "langs" .= fmap (translationLanguage . entityVal) translations
        ]

  return $ Object $ H.union e $ H.filterWithKey (\k _ -> elem k expose) r

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
