{-# LANGUAGE FlexibleInstances #-}
module Handler.Home where

import           Data.Aeson           (Object, decode, encode)
import           Data.Aeson           ((.:?))
import           Data.Aeson.Types     (Parser, parseMaybe)
import qualified Data.Text            as T (replace)
import           Data.Time.Format     (readTime)
import           Database.Persist.Sql (fromSqlKey)
import           Import

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Value
getHomeR = do
     returnJson ()

doInstall :: AppSettings -> SqlPersistT IO ()
doInstall appSettings = do
    exifFile <- liftIO $ readFile "exif.json"
    let maybeExif = decode exifFile :: Maybe [Object]
    case maybeExif of
        Nothing -> return ()
        Just exif -> do
            _ <- sequence $ map createPhoto exif
            return ()
            where
                createPhoto :: Object -> SqlPersistT IO ()
                createPhoto obj = do
                    let meta = appMetadata appSettings
                    categories <- fromMaybe (return []) $ flip parseMaybe obj $ \o -> do
                         categories <- o  .: "IPTC:Keywords"  :: Parser [Text]
                         return $ sequence $ flip map categories $ \title -> do
                            let normalizedTitle = unpack $ T.replace " " "-" (toLower title)
                            let category = Category normalizedTitle Nothing
                            putStrLn $ "insertBy: " ++ (pack $ show category)
                            ecid <- insertBy category
                            let cid = either entityKey id ecid
                            let translation = Translation En CategoryType (fromSqlKey cid) "title" title
                            putStrLn $ "upsert: " ++ (pack $ show translation)
                            _    <- upsert translation [TranslationValue =. title]
                            return cid

                    maybePhoto <- fromMaybe (return Nothing) $ flip parseMaybe obj $ \o -> do
                         name           <- o  .:  (name meta) :: Parser String
                         src            <- o  .:  (src meta)  :: Parser Text
                         width          <- o  .:  (width meta)
                         height         <- o  .:  (height meta)
                         author         <- o  .:? (author meta)
                         caption        <- o  .:? (caption meta) :: Parser (Maybe (Text))
                         dateString     <- o  .:? (date meta)

                         let thumb  = Just $ unpack $ T.replace "static/gallery" "static/thumb" src
                         let exifData = toStrict $ decodeUtf8 $ encode obj
                         let datetime = flip fmap dateString $ \ds -> readTime defaultTimeLocale "%Y:%m:%d %H:%I:%S" ds :: UTCTime
                         let insertPhoto = do
                              aid <- maybe (return Nothing) persistAuthor author
                              let photo = Photo name (unpack src) thumb width height exifData 0 aid datetime Nothing
                              putStrLn $ "insertBy: " ++ (pack $ show photo)
                              epid <- insertBy photo
                              let pid = either entityKey id epid
                              _ <- return $ liftM (persistTranslation pid) caption
                              return  $ Just pid
                              where
                                persistAuthor :: String -> SqlPersistT IO (Maybe (Key Author))
                                persistAuthor a = do
                                  let ea = Author a
                                  putStrLn $ "insertBy: " ++ (pack $ show ea)
                                  eaid <- insertBy ea
                                  return $ Just (either entityKey id eaid)

                                persistTranslation :: Key Photo -> Text -> SqlPersistT IO (Entity Translation)
                                persistTranslation pid c = do
                                    let translation = Translation En PhotoType (fromSqlKey pid) "caption" c
                                    putStrLn $ "upsert: " ++ (pack $ show translation)
                                    upsert translation [TranslationValue =. c]

                         return insertPhoto

                    case maybePhoto of
                        Just pid -> do
                            _ <- sequence $ flip map categories $ \cid -> insertUnique $ PhotoCategory cid pid
                            return ()
                        Nothing ->
                            return ()
