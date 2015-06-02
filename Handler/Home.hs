{-# LANGUAGE FlexibleInstances #-}
module Handler.Home where

import           Data.Aeson           (Object, decode, eitherDecode, encode, withObject, (.:?))
import           Data.Aeson.Types     (parseMaybe)
import qualified Data.Text            as T (replace)
import           Data.Time.Format     (readTime)
import           Database.Persist.Sql (fromSqlKey)
import           Import
-- import qualified Data.ByteString.Lazy.Internal      as B (ByteString)

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

data ExifData = ExifData {
    categories  :: [Text],
    name        :: String,
    src         :: Text,
    width       :: Int,
    height      :: Int,
    author      :: Maybe (String),
    caption     :: Maybe (Text),
    date        :: Maybe (String),
    exifObject    :: Object
} deriving Show

instance FromJSON ExifData where
    parseJSON = withObject "ExifData" $ \o -> do
        let exifObject = o
        categories  <- o .:  "IPTC:Keywords"
        name        <- o .:  "File:FileName"
        src         <- o .:  "SourceFile"
        width       <- o .:  "File:ImageWidth"
        height      <- o .:  "File:ImageHeight"
        author      <- o .:? "EXIF:Artist"
        caption     <- o .:? "EXIF:Artist"
        date        <- o .:? "EXIF:CreateDate"

        return ExifData {..}

doInstall :: SqlPersistT IO ()
doInstall = do
    exifFile <- liftIO $ readFile "exif.json"
    let eresult = eitherDecode exifFile :: Either String [Value]
    case eresult of
        Left err -> liftIO $ putStrLn $ pack err
        Right objects -> do 
            let exif = map (parseMaybe parseJSON) objects :: [Maybe ExifData]
            _ <- sequence $ map persistData (catMaybes exif)
            return ()
    return ()

    where
        persistData :: ExifData -> SqlPersistT IO ()
        persistData exif = do
            aid <- maybe (return Nothing) persistAuthor (author exif)
            categories <- sequence $ map persistCategory (categories exif)
            pid <- persistPhoto exif aid
            _ <- sequence $ flip map categories $ \cid -> insertUnique $ PhotoCategory cid pid
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
                        category = Category normalizedTitle Nothing
                    ecid <- insertBy category
                    let cid = either entityKey id ecid
                    _ <- persistTranslation CategoryType (fromSqlKey cid) "title" title
                    return cid

                persistPhoto :: ExifData -> Maybe (Key Author) -> SqlPersistT IO (Key Photo)
                persistPhoto e aid = do
                    let thumb  = Just $ unpack $ T.replace "static/src" "static/thumb" (src e)
                        exifData = toStrict $ decodeUtf8 $ encode (exifObject e)
                        datetime = flip fmap (date e) $ \ds -> readTime defaultTimeLocale "%Y:%m:%d %H:%I:%S" ds :: UTCTime
                        photo = Photo (name e) (unpack $ src e) thumb (width e) (height e) exifData 0 aid datetime Nothing
                    epid <- insertBy photo
                    let pid = either entityKey id epid
                    _ <- maybe (return Nothing) (persistTranslation PhotoType (fromSqlKey pid) "caption") (caption exif)
                    return pid