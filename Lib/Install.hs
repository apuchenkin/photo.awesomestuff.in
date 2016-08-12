{-# LANGUAGE FlexibleInstances #-}

module Lib.Install where

import           Data.Aeson           (Object, eitherDecode, encode, withObject, (.:?))
import           Data.Aeson.Types     (parseMaybe)
import qualified Data.Text            as T (replace)
import           Data.Time.Format     (readTime)
import           Database.Persist.Sql (fromSqlKey)
import           Import
import Model

data ExifData = ExifData {
    categories  :: Either Text [Text],
    name        :: String,
    src         :: Text,
    width       :: Int,
    height      :: Int,
    author      :: Maybe String,
    caption     :: Maybe Text,
    date        :: Maybe String,
    exifObject  :: Object
} deriving Show

instance FromJSON ExifData where
    parseJSON = withObject "ExifData" $ \o -> do
        let exifObject = o
        categories  <-
          (   (Left  <$> o .: "IPTC:Keywords")
          <|> (Right <$> o .: "IPTC:Keywords")
          )
        name        <- o .:  "File:FileName"
        src         <- o .:  "SourceFile"
        width       <- o .:  "File:ImageWidth"
        height      <- o .:  "File:ImageHeight"
        author      <- o .:? "EXIF:Artist"
        caption     <- o .:? "EXIF:ImageDescription"
        date        <- o .:? "EXIF:CreateDate"

        return ExifData {..}

doInstall :: SqlPersistT IO ()
doInstall = do
    print "read: exif.json"
    exifFile <- liftIO $ readFile "exif.json"
    let eresult = eitherDecode exifFile :: Either String [Value]
    case eresult of
        Left err -> liftIO $ putStrLn $ pack err
        Right objects -> do
            let exif = map (parseMaybe parseJSON) objects :: [Maybe ExifData]
            _ <- mapM persistData (catMaybes exif)
            return ()
    return ()

    where
        persistData :: ExifData -> SqlPersistT IO ()
        persistData exif = do
            print exif
            let categoriesList = either singleton id (categories exif)
            aid <- maybe (return Nothing) persistAuthor (author exif)
            categories <- mapM persistCategory categoriesList
            pid <- persistPhoto exif aid
            _   <- mapM (\cid -> insertUnique $ PhotoCategory cid pid) categories
            return ()

            where
                persistTranslation :: TranslationType -> Int64 -> String -> Text -> SqlPersistT IO (Maybe (Key Translation))
                persistTranslation t refId f v = do
                    let translation = Translation En t refId f v
                    insertUnique translation

                persistAuthor :: String -> SqlPersistT IO (Maybe (Key Author))
                persistAuthor a = do
                    eaid <- insertBy $ Author a
                    return $ Just (either entityKey id eaid)

                persistCategory :: Text -> SqlPersistT IO (Key Category)
                persistCategory title = do
                    let normalizedTitle = unpack $ T.replace " " "-" (toLower title)
                        category = Category normalizedTitle Nothing False Nothing Nothing
                    ecid <- insertBy category
                    let cid = either entityKey id ecid
                    _ <- persistTranslation CategoryType (fromSqlKey cid) "title" title
                    return cid

                persistPhoto :: ExifData -> Maybe (Key Author) -> SqlPersistT IO (Key Photo)
                persistPhoto e aid = do
                    let thumb  = Just $ unpack $ T.replace "static/src" "static/thumb" (src e)
                        exifData = toStrict $ decodeUtf8 $ encode (exifObject e)
                        datetime = flip fmap (date e) $ \ds -> readTime defaultTimeLocale "%Y:%m:%d %H:%I:%S" ds :: UTCTime
                        photo = Photo
                            (name e)            -- name
                            (unpack $ src e)    -- src
                            thumb               -- thumb
                            (width e)           -- width
                            (height e)          -- height
                            exifData            -- exif
                            0                   -- views
                            aid                 -- author
                            datetime            -- datetime
                            Nothing             -- order
                            False               -- hidden
                            Nothing             -- group

                    epid <- insertBy photo
                    let pid = either entityKey id epid
                    _ <- maybe (return Nothing) (persistTranslation PhotoType (fromSqlKey pid) "caption") (caption exif)
                    return pid
