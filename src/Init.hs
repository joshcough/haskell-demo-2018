
module Init where

import           Control.Concurrent.Chan     (newChan)
import           Control.Exception           (bracket, throwIO)
import           Control.Lens                ((<&>), set)
import           Control.Monad.Logger                 (runNoLoggingT)
import           Control.Monad.Trans.AWS     (Credentials( Discover ), LogLevel( Debug ), envLogger, newEnv, newLogger)
import           Data.Monoid                 ((<>))
import qualified Data.Pool             as Pool
import           Data.Text                   (Text, pack, unpack)
import qualified Data.Text             as T
import           Network.HTTP.Nano           (HttpCfg(..), tlsManager)
import           Network.Wai                 (Application)
import           Network.Wai.Handler.Warp    (run)
import           Safe                        (readMay)
import           Servant.Auth.Server         (defaultCookieSettings, defaultJWTSettings) --, generateKey)
import           System.Environment          (getEnv, lookupEnv)
import           System.IO                   (stdout)
import           Web.Rollbar                 (RollbarCfg(..))
import qualified Web.Rollbar                 as RB

import           Database.Persist.Postgresql          (ConnectionPool, createPostgresqlPool)
import           Database.PostgreSQL.Simple.Internal  (postgreSQLConnectionString)
import           Database.PostgreSQL.Simple.URL       (parseDatabaseUrl)

import           Api                         (app)
import           Logging                     (LoggingCfg)
import qualified Logging
import           Config                      (AwsConfig(..), Config (..), Environment(..))
import qualified KeyGen                as KG

-- | An action that creates a WAI 'Application' together with its resources,
--   runs it, and tears it down on exit
runApp :: IO ()
runApp = bracket acquireConfig shutdownApp runApp'
  where
    runApp' config = run (_configPort config) =<< initialize config

-- | The 'initialize' function accepts the required environment information,
-- initializes the WAI 'Application' and returns it
initialize :: Config -> IO Application
initialize = pure . app

-- | Allocates resources for 'Config'
acquireConfig :: IO Config
acquireConfig = do
    _configPort         <- lookupSetting "PORT" 8081
    _configEnv          <- lookupSetting "ENV" Development
    _configPool         <- do dbUrl <- getEnv "DATABASE_URL"
                              makePool dbUrl _configEnv
    _configWebSocketEnv <- newChan
    let _configCookies  = defaultCookieSettings
    _configJWT          <- defaultJWTSettings <$> case _configEnv of
        Production  -> KG.readProdKey
        Development -> KG.readDevKey
        Test        -> KG.readTestKey
    _configHttp         <- mkHttp
    _configAwsEnv       <- acquireAwsConfig
    _configRollbar      <- mkRollbar
    _configLogging      <- mkLoggingCfg
    pure Config {..}

-- | Allocates resources for AwsConfig
acquireAwsConfig :: IO AwsConfig
acquireAwsConfig = do
    _awsConfigS3RootUrl               <- lookupSetting' "AWS_S3_ROOT_URL" "https://s3.amazonaws.com/"
    _awsConfigDemoBucketName    <- lookupSetting' "DEMO_BUCKET" "demo"
    let _awsConfigDemoBucketUrl = _awsConfigS3RootUrl <> _awsConfigDemoBucketName <> "/"
    _awsConfigEnv                     <- do lgr <- newLogger Debug stdout
                                            newEnv Discover <&> set envLogger lgr
    return AwsConfig {..}

-- | Takes care of cleaning up 'Config' resources
shutdownApp :: Config -> IO ()
shutdownApp Config {..} = do
    Pool.destroyAllResources _configPool
    pure ()

-- | Looks up a text setting in the environment, with a provided default
lookupSetting' :: Text -> Text -> IO Text
lookupSetting' env def = maybe def pack <$> lookupEnv (unpack env)

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: Read a => Text -> a -> IO a
lookupSetting env def = do
    maybeValue <- fmap pack <$> lookupEnv (unpack env)
    maybe (return def) (\str -> maybe (handleFailedRead str) return (readMay $ unpack str)) maybeValue
  where
    handleFailedRead str =
        error . unpack $ mconcat
            [ "Failed to read [["
            , str
            , "]] for environment variable "
            , env
            ]

-- | rollbar
mkRollbar :: IO RollbarCfg
mkRollbar = RollbarCfg  <$> (RB.AccessToken . T.pack <$> getEnv "ROLLBAR_TOKEN")
                        <*> (RB.Environment . T.pack <$> getEnv "ROLLBAR_ENVIRONMENT")
                        <*> (fmap . fmap) (RB.Host . T.pack) (lookupEnv "ROLLBAR_HOST")
                        <*> (fmap . fmap) (RB.CodeVersion . T.pack) (lookupEnv "SOURCE_VERSION")
                        <*> (maybe False read <$> lookupEnv "ROLLBAR_MUTE")

mkHttp :: IO HttpCfg
mkHttp = HttpCfg <$> tlsManager

mkLoggingCfg :: IO LoggingCfg
mkLoggingCfg = Logging.fromEnv

-- | This function creates a 'ConnectionPool' for the given environment.
makePool :: String -> Environment -> IO ConnectionPool
makePool dbUrl env = case postgreSQLConnectionString <$> parseDatabaseUrl dbUrl of
    Nothing  -> throwIO (userError "DATABASE_URL malformed.")
    Just url -> runNoLoggingT $ createPostgresqlPool url (envPool env)

-- | The number of pools to use for a given environment.
envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8
