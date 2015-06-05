module Handler.Static where

import           Crypto.Hash            (HMAC, SHA1, digestToHexByteString,
                                         hmac, hmacGetDigest)
import qualified Data.ByteString.Base64 as B64 (decodeLenient)
import           Data.ByteString.UTF8   (toString)
import           Data.Ratio             ((%))
import           Database.Persist.Sql   (fromSqlKey)
import           Import
import           OpenCV.Core.CV         (InterpolationMethod (..), cvResize)
import           OpenCV.Core.CxCore     (CvSize (..), cvCreateImage,
                                         cvGetSize, cvReleaseImage, getDepth,
                                         getNumChannels)
import           OpenCV.Core.HighGui    (LoadColor (..), cvLoadImage,
                                         cvSaveImage)
import           System.Directory       (doesFileExist)
import           System.FilePath.Posix  (takeExtension)


remap :: Int -> Int -> Int -> Int -> (Int, Int)
remap h w h' w'
    | r > 1 = (h', w * h' `div` h)
    | r < 1 = (w' * h `div` w, w')
    | otherwise = (h', w')
    where r = (w'* h) % (w * h')

checkSign :: String -> String -> String -> Bool
checkSign phrase secret sign = sign == sign'
    where
        digest  = hmac (fromString secret) $ fromString phrase :: HMAC SHA1
        sign'   = toString $ digestToHexByteString $ hmacGetDigest digest

openCVresize :: String -> String -> Int -> Int -> IO ()
openCVresize src dest w h = do
    image   <- cvLoadImage src LoadUnchanged
    nch     <- getNumChannels image
    depth   <- getDepth image
    image'  <- cvCreateImage (CvSize (fromIntegral w) (fromIntegral h)) nch depth
    cvResize image image' CV_INTER_AREA
    cvSaveImage dest image'
    cvReleaseImage image
    cvReleaseImage image'

getStaticPhotoR :: PhotoId -> Int -> Int -> String -> Handler TypedContent
getStaticPhotoR pid w' h' sign = do
    app <- getYesod
    let settings = appSettings app
        phrase   = show (fromSqlKey pid) ++ "-" ++ show w' ++ "x" ++ show h'
    _ <- if checkSign phrase (secret settings) sign then return () else notFound

    mphoto <- runDB $ get pid
    case mphoto of {Nothing -> notFound; Just photo -> processPhoto (cachePath settings) photo}
    where
        processPhoto :: String -> Photo -> Handler TypedContent
        processPhoto cachePath photo = do
            let (w, h)      = (photoWidth photo, photoHeight photo)
                (h'', w'')  = remap h w h' w'
                pth         = show (fromSqlKey pid) ++ "-" ++ show w'' ++ "x" ++ show h''
                cacheFile   = cachePath ++ pth ++ takeExtension (photoSrc photo)

            neverExpires
            isCached    <- liftIO $ doesFileExist cacheFile
            if isCached
                then sendFile typeJpeg cacheFile
                else do
                    liftIO $ openCVresize (photoSrc photo) cacheFile w'' h''
                    sendFile typeJpeg cacheFile

getStaticImageR :: String -> Int -> Int -> String -> Handler TypedContent
getStaticImageR encpath w' h' s = processPhoto (B64.decodeLenient $ fromString encpath)
    where
        processPhoto :: ByteString -> Handler TypedContent
        processPhoto bpath = do
            app <- getYesod

            let settings = appSettings app
                path    = toString bpath
                pth     = encpath ++ "-" ++ show w' ++ "x" ++ show h'
                digest  = hmac (fromString $ secret settings) $ fromString pth :: HMAC SHA1
                sign = digestToHexByteString $ hmacGetDigest digest
                cacheFile = cachePath settings ++ pth ++ takeExtension path

            _           <- if sign == fromString s then return () else notFound
            isCached    <- liftIO $ doesFileExist cacheFile
            neverExpires
            if isCached
                then sendFile typeJpeg cacheFile
                else saveCache path cacheFile

            where
                saveCache :: String -> String -> Handler TypedContent
                saveCache path cachePath = do
                    image <- liftIO $ cvLoadImage path LoadUnchanged
                    size  <- liftIO $ cvGetSize image
                    let (w, h) = (sizeWidth size, sizeHeight size)
                    let (h'', w'') = remap (fromIntegral h) (fromIntegral w) (fromIntegral h') (fromIntegral w')
                    liftIO $ cvReleaseImage image
                    liftIO $ openCVresize path cachePath w'' h''

                    sendFile typeJpeg cachePath
