module Handler.Static where

import Import
import           Database.Persist.Sql   (fromSqlKey)
import qualified Data.ByteString.Base64  as B64 (decodeLenient)
import System.Directory                 (doesFileExist)
import System.FilePath.Posix            (takeExtension)
import Data.ByteString.UTF8             (toString)
import OpenCV.Core.CV                   (cvResize, InterpolationMethod (..))
import OpenCV.Core.HighGui              (cvLoadImage, cvSaveImage, LoadColor (..))
import OpenCV.Core.CxCore               (cvCreateImage, cvGetSize, CvSize (..), getNumChannels, getDepth, cvFree, cvReleaseImage)
import Data.Ratio                       ((%))
import Crypto.Hash                      (hmac, HMAC, SHA1, hmacGetDigest, digestToHexByteString)


remap :: Int -> Int -> Int -> Int -> (Int, Int)
remap h w h' w'
    | r > 1 = (h', w * h' `div` h)
    | r < 1 = (w' * h `div` w, w')
    | otherwise = (h', w')
    where r = (w'* h) % (w * h')

checkSign :: String -> String -> String -> Bool
checkSign phrase secret sign = (sign == sign')
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
        phrase   = (show $ fromSqlKey pid) ++ "-" ++ show w' ++ "x" ++ show h'
    _ <- case (checkSign phrase (secret settings) sign) of {False -> notFound; True -> return ()}

    mphoto <- runDB $ get pid
    case mphoto of {Nothing -> notFound; Just photo -> processPhoto (cachePath settings) photo}
    where
        processPhoto :: String -> Photo -> Handler TypedContent
        processPhoto cachePath photo = do
            let (w, h)      = (photoWidth photo, photoHeight photo)
                (h'', w'')  = remap h w h' w'
                pth         = (show $ fromSqlKey pid) ++ "-" ++ show w'' ++ "x" ++ show h''
                cacheFile   = cachePath ++ pth ++ (takeExtension (photoSrc photo))

            neverExpires
            isCached    <- liftIO $ doesFileExist cacheFile
            case isCached of
                True    -> sendFile typeJpeg cacheFile
                False   -> do
                    liftIO $ openCVresize (photoSrc photo) cacheFile w'' h''
                    sendFile typeJpeg cachePath


getStaticImageR :: String -> Int -> Int -> String -> Handler TypedContent
getStaticImageR encpath w' h' s = do
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
            neverExpires
            case isCached of
                True    -> sendFile typeJpeg cacheFile
                False   -> saveCache path cacheFile
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
