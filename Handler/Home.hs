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

doInstall :: SqlPersistT IO ()
doInstall = do
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
                    categories <- fromMaybe (return []) $ flip parseMaybe obj $ \o -> do
                         categories <- o  .: "IPTC:Keywords"  :: Parser [Text]
                         return $ sequence $ flip map categories $ \title -> do
                            let normalizedTitle = unpack $ T.replace " " "-" (toLower title)
                            ecid <- insertBy $ Category normalizedTitle Nothing
                            let cid = either entityKey id ecid
                            _    <- insertUnique $ Translation En CategoryType (fromSqlKey cid) "title" (unpack $ title)
                            return cid

                    maybePhoto <- fromMaybe (return Nothing) $ flip parseMaybe obj $ \o -> do
                         name           <- o  .:  "File:FileName" :: Parser String
                         src            <- o  .:  "SourceFile"
                         width          <- o  .:  "File:ImageWidth"
                         height         <- o  .:  "File:ImageHeight"
                         dir            <- o  .:  "File:Directory"
                         author         <- o  .:? "EXIF:Artist"
                         caption        <- o  .:? "IPTC:Caption-Abstract"
                         dateString     <- o  .:? "EXIF:CreateDate"

                         let thumb  = Just (dir ++ "/thumb/" ++ name)
                         let exifData = toStrict $ decodeUtf8 $ encode obj
                         let datetime = flip fmap dateString $ \ds -> readTime defaultTimeLocale "%Y:%m:%d %H:%I:%S" ds :: UTCTime
                         let insertPhoto = do
                              aid <- maybe (return Nothing) persistAuthor author
                              let photo = Photo name src thumb width height exifData 0 aid datetime Nothing
                              pid <- insertUnique photo
                              _ <- return $ liftM2 persistTranslation pid caption
                              return pid
                              where
                                persistAuthor :: String -> SqlPersistT IO (Maybe (Key Author))
                                persistAuthor a = do
                                  eaid <- insertBy $ Author a
                                  return $ Just (either entityKey id eaid)

                                persistTranslation :: Key Photo -> String -> SqlPersistT IO (Maybe (Key Translation))
                                persistTranslation pid c = insertUnique $ Translation En PhotoType (fromSqlKey pid) "caption" c

                         return insertPhoto

                    case maybePhoto of
                        Just pid -> do
                            _ <- sequence $ flip map categories $ \cid -> insertUnique $ PhotoCategory cid pid
                            return ()
                        Nothing ->
                            return ()
