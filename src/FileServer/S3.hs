
module FileServer.S3 (
    S3FileServer(..)
  , UploadFile(..)
  ) where

import           Control.Monad                (void)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Reader         (asks)
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Aeson                   ((.=))
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import           Database.Esqueleto           (Entity(..), fromSqlKey)
import           Network.AWS.S3               (ObjectKey(..), BucketName(..), deleteObject, putObject)

import           Config                       (AppT', Config(..), AwsConfig(..))
import           FileServer.DatabaseModels    (DbFile(..))
import           FileServer.Models            (File(..))
import           Logging                      (logDebug)
import           Util.Utils                   (tShow)


{-
For local testing, run this:

import Config (AppT, runAppT)
import Init (acquireConfig)
import Database.Esqueleto (toSqlKey, Entity(..))
import qualified FileServer.S3 as S3
import FileServer.DatabaseModels
let f = DbFile (toSqlKey 100) (toSqlKey 200) "Setup.hs"
(_, c) <- acquireConfig
runAppT (S3.uploadFile (S3.UploadFile (Entity (toSqlKey 999) f) "Setup.hs") :: AppT IO File) c
runAppT (configBucket) c
-}

class Monad m => S3FileServer m where
    uploadFile :: UploadFile -> m File
    deleteFile :: Entity DbFile -> m File
    withS3Paths :: Entity DbFile -> m File

instance (MonadIO m) => S3FileServer (AppT' e m) where
    uploadFile (UploadFile dbFile filePath) = do
        f@File{..} <- withS3Paths dbFile
        putChunkedFile (ObjectKey fileS3File) filePath
        return f

    deleteFile dbFile = do
        f@File{..} <- withS3Paths dbFile
        $(logDebug) "Deleting file in aws" ["fileS3Path" .= fileS3Path]
        withBucket $ \bucket -> deleteObject bucket (ObjectKey fileS3File)
        return f

    withS3Paths e = do
        s3Bucket <- _awsConfigDemoBucketUrl <$> asks _configAwsEnv
        return $ entityToFile s3Bucket e

-- |
data UploadFile = UploadFile {
    _uploadFileFile          :: Entity DbFile
  , _uploadFileLocalFilePath :: FilePath
  } deriving (Eq, Show)

-- |
putChunkedFile :: (MonadIO m) => ObjectKey -> FilePath -> AppT' e m ()
putChunkedFile k f = do
    $(logDebug) "Uploading file to aws" ["objectKey" .= show k, "filepath" .= f]
    bdy <- chunkedFile defaultChunkSize f
    withBucket $ \bucket -> putObject bucket k bdy

-- |
withBucket :: (AWSRequest r, Show r, MonadIO m) => (BucketName -> r) -> AppT' e m ()
withBucket msg = configBucket >>= \bucket -> do
    $(logDebug) "Sending request to aws" ["request" .= show (msg bucket)]
    runAwsAction . void $ send (msg bucket)
    where

    configBucket :: Monad m => AppT' e m BucketName
    configBucket = BucketName . _awsConfigDemoBucketName <$> asks _configAwsEnv

    runAwsAction :: MonadIO m => AWST' Env (ResourceT IO) a -> AppT' e m a
    runAwsAction m = do
        env <- _awsConfigEnv <$> asks _configAwsEnv
        liftIO $ runResourceT . runAWST env $ m

-- |
entityToFile :: Text -> Entity DbFile -> File
entityToFile s3BucketUrl (Entity k DbFile {..}) = File {
    fileId       = fileId
  , fileUserId   = userId
  , fileName     = dbFileOriginalFileName
  , fileS3File   = s3File
  , fileS3Path   = s3BucketUrl <> s3File
  }
  where fileId = fromSqlKey k
        userId = fromSqlKey dbFileUserId
        s3File = tShow userId <> "/" <> tShow fileId <> "/" <> dbFileOriginalFileName
