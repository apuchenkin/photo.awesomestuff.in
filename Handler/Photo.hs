{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.Photo where

import Import
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import qualified Data.ByteString.Base64  as B64 (decodeLenient)
import System.Directory                  (doesFileExist, createDirectoryIfMissing)
import System.FilePath.Posix             (takeDirectory, takeExtension)
import Data.ByteString.UTF8             (toString)
import Vision.Image.Storage.DevIL       (JPG (..), load, save)
import Vision.Primitive.Shape
import Vision.Image.RGB.Type
import Vision.Image.Type
import Vision.Image.Transform
import Data.Ratio                       ((%))
import Crypto.Hash                      (hmac, HMAC, SHA1, hmacGetDigest, digestToHexByteString)


getImageR :: String -> Int -> Int -> String -> Handler TypedContent
getImageR encpath w' h' s = do
    processPhoto (B64.decodeLenient $ fromString encpath)
    where
        processPhoto :: ByteString -> Handler TypedContent
        processPhoto bpath = do
            app <- getYesod

            let settings = appSettings app
                path    = toString bpath
                pth     = encpath ++ "-" ++ show w' ++ "x" ++ show h'
                digest  = hmac (fromString $ secret settings) $ fromString pth :: HMAC SHA1
                sign = digestToHexByteString $ hmacGetDigest digest
                cacheFile = (cachePath settings) ++ pth ++ (takeExtension path)

            _           <- case (sign == fromString s) of {False -> notFound; True -> return ()}
            isCached    <- liftIO $ doesFileExist cacheFile

            case isCached of
                True    -> sendFile typeJpeg cacheFile
                False   -> saveCache path cacheFile
            where
                saveCache :: String -> String -> Handler TypedContent
                saveCache path cachePath = do
                    io <- liftIO $ load JPG path
                    case io of
                        Right (rgb :: RGB) -> do
                            let Z :. h :. w = manifestSize rgb
                            let (h'', w'') = remap h w h' w'
                            let rs = resize Bilinear (ix2 h'' w'') rgb :: RGB
                            liftIO $ createDirectoryIfMissing True (takeDirectory cachePath)
                            mErr <- liftIO $ save JPG cachePath rs
                            case mErr of
                                Nothing  -> sendFile typeJpeg cachePath
                                Just err -> invalidArgs ["unable to save image: " ++ pack cachePath, pack $ show err]

                        Left _ -> notFound

remap :: Int -> Int -> Int -> Int -> (Int, Int)
remap h w h' w'
    | r > 1 = (h', w * h' `div` h)
    | r < 1 = (w' * h `div` w, w')
    | otherwise = (h', w')
    where r = (w'* h) % (w * h')

getPhotoR :: PhotoId -> Handler Value
getPhotoR photoId = do
    photo <- runDB $ get photoId
    let mViews = fmap succ (fmap photoViews photo)
    runDB $ update photoId [PhotoViews  =. case mViews of {Nothing -> 0; Just views -> views}]
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