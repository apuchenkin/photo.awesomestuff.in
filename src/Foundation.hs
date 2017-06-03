{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Foundation where

import Import.NoFoundation
import Database.Persist.Sql             (ConnectionPool, runSqlPool)
import Yesod.Core.Types                 (Logger)
import Data.ByteString.Base64           (decodeLenient)
import Data.Word8                       (_space, _colon)
import Data.Aeson                       (eitherDecodeStrict)
import qualified Yesod.Auth.Message     as Msg
import qualified Yesod.Core.Unsafe      as Unsafe
import qualified Network.Wai.Internal   as W (requestHeaders, requestBody)
import qualified Data.ByteString        as B
import qualified Crypto.Hash            as C (hash, SHA256, Digest, digestToHexByteString)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
-- type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  -- Controls the base of generated URLs. For more information on modifying,
  -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Sessions is disabled
    makeSessionBackend _ = return Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _  = return Authorized
    isAuthorized FaviconR _   = return Authorized
    isAuthorized RobotsR _    = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ False      = return Authorized
    isAuthorized _ _ = do
        mu <- maybeAuthId
        return $ case mu of
            Nothing -> AuthenticationRequired
            Just _ -> Authorized

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger
    yesodMiddleware handler = do
        authorizationCheck
        handler

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    authPlugins _   = []
    loginDest _     = HomeR
    logoutDest _    = HomeR
    authHttpManager = error "authHttpManager"

    authenticate creds = runDB $ do
        muid <- getBy $ UniqueUser $ credsIdent creds
        case muid of
          Nothing -> return $ UserError Msg.InvalidLogin
          Just uid -> do
            let p = lookup "password" $ credsExtra creds
            return $ case p == userPassword (entityVal uid) of
              False -> UserError Msg.InvalidLogin
              True  -> Authenticated $ entityKey uid

    maybeAuthId = do
        request <- waiRequest
        let authorization = lookup "Authorization" (W.requestHeaders request)
        case authorization of
            Just header -> do
                let (_, v) = B.breakByte _space header
                let (username, password) = B.breakByte _colon $ decodeLenient v
                case B.uncons password of
                    Just (_, p) -> do
                        let hash = C.hash p :: C.Digest C.SHA256
                        auth <- authenticate $ Creds "" (decodeUtf8 username) [("password", decodeUtf8 $ C.digestToHexByteString hash)]
                        return $ case auth of
                            Authenticated auid -> Just auid
                            _ -> Nothing
                    _ -> return Nothing
            _ -> return Nothing

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

getRequestBody :: FromJSON a => Handler a
getRequestBody = do
  request <- waiRequest
  body    <- liftIO $ W.requestBody request
  let result = eitherDecodeStrict body
  case result of
    Left  err     -> invalidArgs [pack err]
    Right value   -> return value
