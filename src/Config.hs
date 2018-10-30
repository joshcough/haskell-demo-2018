{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Config (
    AppT
  , AppT'
  , App
  , AwsConfig(..)
  , Config(..)
  , Environment(..)
  , runAppT
  , runAppTInTest
  , runDb
  -- lenses for config
  , configPool, configEnv, configPort, configCookies, configJWT, configHttp, configRollbar
  ) where

import           Control.Monad.Except                 (liftIO)
import           Control.Lens                         ((^.))
import           Control.Lens.TH                      (makeClassy)
import           Control.Monad                        (when)
import           Control.Monad.Except                 (ExceptT(..), runExceptT)
import           Control.Monad.Reader                 (MonadIO, MonadReader, ReaderT(..), asks)
import qualified Control.Monad.Trans.AWS              as AWS
import           Data.Aeson                           ((.=))
import           Data.Text                            (Text)
import           Database.Persist.Postgresql          (ConnectionPool)
import           Database.Persist.Sql                 (SqlPersistT, runSqlPool)
import           Network.HTTP.Nano                    (HasHttpCfg(..), HttpCfg(..), HttpError)
import           Network.Wai.Handler.Warp             (Port)
import           Servant.Auth.Server
import           Web.Rollbar                          (RollbarCfg(..), HasRollbarCfg(..), ToRollbarEvent(..), rollbar)

import           Error
import           Logging
import           Util.Utils                           (tShow)

-- | Right now, we're distinguishing between three environments. We could
-- also add a @Staging@ environment if we needed to.
data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read)

-- | The AWS Config for our application
data AwsConfig = AwsConfig {
    _awsConfigS3RootUrl            :: Text
  , _awsConfigProverlaysBucketName :: Text
  , _awsConfigProverlaysBucketUrl  :: Text
  , _awsConfigEnv                  :: AWS.Env
  }
makeClassy ''AwsConfig

-- | The Config for our application
data Config
    = Config
    { _configPool         :: ConnectionPool
    , _configEnv          :: Environment
    , _configPort         :: Port
    , _configCookies      :: CookieSettings
    , _configJWT          :: JWTSettings
    , _configHttp         :: HttpCfg
    , _configAwsEnv       :: AwsConfig
    , _configRollbar      :: RollbarCfg
    , _configLogging      :: LoggingCfg
    }

makeClassy ''Config

instance HasHttpCfg Config where
    httpCfg = configHttp

instance HasRollbarCfg Config where
    rollbarCfg = configRollbar

instance HasLoggingCfg Config where
    loggingCfg = configLogging

---
---
---

type AppT m = AppT' ProverlaysError m
type AppT' e m = ReaderT Config (ExceptT e (LoggingJSONT m))

type App = AppT IO

-- | Executes the given computation in AppT, logging to stdout with log level configured in the
--   context (via `HasLoggingCfg`), and sending errors to Rollbar (using `HasRollbarCfg`).
runAppT :: forall err a. (ClassifiedError err, ToRollbarEvent err, Show err) => AppT' err IO a -> Config -> IO (Either err a)
runAppT = runAppT' $ \err -> do
    $(logError) "Uncaught app error" ["error" .= tShow err]
    when (isUnexpected err) (rollbar $ toRollbarEvent err)

-- | Runs without rollbar
runAppTInTest :: forall err a. (ClassifiedError err, ToRollbarEvent err, Show err) => AppT' err IO a -> Config -> IO (Either err a)
runAppTInTest = runAppT' $ \err -> $(logError) "Uncaught app error" ["error" .= tShow err]

-- |
runAppT' :: forall err a. (ClassifiedError err, ToRollbarEvent err, Show err) =>
       (err -> AppT' HttpError IO ())
    -> AppT' err IO a
    -> Config
    -> IO (Either err a)
runAppT' onError action context = do
    let run :: forall e r. AppT' e IO r -> IO (Either e r)
        run f = (runStdoutLoggingJSONT level sourceVersion . runExceptT) (runReaderT f context)
        handleErrorCallingRollbar :: Either HttpError () -> IO ()
        handleErrorCallingRollbar = either print (const $ return ())
    res <- run action
    either (\e -> run (onError e) >>= handleErrorCallingRollbar) (const $ pure ()) res
    pure res
    where
        level = context ^. (loggingCfg . logLevel)
        sourceVersion = context ^. (loggingCfg . logSourceVersion)

-- |
runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = asks _configPool >>= liftIO . runSqlPool query
