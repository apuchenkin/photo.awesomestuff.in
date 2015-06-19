module Handler.Category where

import Import
import qualified Data.HashMap.Strict     as H (union)
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.), (?.))
import qualified Database.Esqueleto.Internal.Sql as EIS (veryUnsafeCoerceSqlExprValue)

getCategoryR :: Handler Value
getCategoryR = do
  cacheSeconds $ 60 * 60 * 24 * 30 -- month
  addHeader "Vary" "Accept-Language, Authorization"
  langs <- languages
  maid  <- maybeAuthId
  result <- runDB
      $ E.select
      $ E.from $ \(category `E.LeftOuterJoin` translation) -> do
        E.on   $ (category ^. CategoryId E.==. unsafeInt64ToKey (translation ?. TranslationRefId))
          E.&&. (translation ?. TranslationLanguage E.==. E.val (Just (pickLanguadge langs)))
          E.&&. (translation ?. TranslationType     E.==. E.val (Just CategoryType))
          E.&&. (translation ?. TranslationField    E.==. E.val (Just "title"))
        case maid of
          Nothing -> E.where_ (category ^. CategoryHidden E.==. E.val False)
          Just _  -> return ()

        return (category, translation ?. TranslationValue)

  returnJson $ map parseResult result
  where
  unsafeInt64ToKey :: E.SqlExpr (E.Value (Maybe Int64)) -> E.SqlExpr (E.Value (Key Category))
  unsafeInt64ToKey = EIS.veryUnsafeCoerceSqlExprValue
  parseResult :: (Entity Category, E.Value (Maybe Text)) -> Value
  parseResult (category, title) = Object $ H.union e c
    where
      (Object c) = toJSON category
      (Object e) = object [
          "title" .= E.unValue title
        ]
