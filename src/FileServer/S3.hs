
module FileServer.S3 (
    S3FileServer(..)
  , UploadFile(..)
  , configBucket
  ) where

import           Control.Lens                 ((&), (.~))
import           Control.Monad                (unless, void)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Reader         (asks)
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Aeson                   ((.=))
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import           Database.Esqueleto           (Entity(..), fromSqlKey)
import           Network.AWS.S3        hiding (bucket)

import           Config                       (AppT', Config(..), AwsConfig(..))
import           FileServer.DatabaseModels    (DbFile(..))
import           FileServer.Models            (File(..), Folder(..), listFilesRecursive)
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
    deleteFolder :: Folder (Entity DbFile) -> m [File]
    withS3Paths :: Entity DbFile -> m File

instance (MonadIO m) => S3FileServer (AppT' e m) where
    uploadFile (UploadFile dbFile filePath) = do
        f@File{..} <- withS3Paths dbFile
        bucket <- configBucket
        putChunkedFile $ PutRequest bucket (ObjectKey fileS3File) filePath
        return f

    deleteFile dbFile = do
        f@File{..} <- withS3Paths dbFile
        $(logDebug) "Deleting file in aws" ["fileS3Path" .= fileS3Path]
        bucket <- configBucket
        runSend $ deleteObject bucket (ObjectKey fileS3File)
        return f

    deleteFolder f = do
        f'@Folder{..} <- mapM withS3Paths f
        let files = listFilesRecursive f'
        unless (null files) $ do
            let fileIds = objectIdentifier . ObjectKey . fileS3File <$> files
            $(logDebug) "Deleting folder in aws " ["folderName" .= folderName, "childFiles" .= files]
            bucket <- configBucket
            runSend $ deleteObjects bucket (delete' & dObjects .~ fileIds)
        return files

    withS3Paths e = do
        s3Bucket <- _awsConfigDemoBucketUrl <$> asks _configAwsEnv
        return $ entityToFile s3Bucket e

-- |
data UploadFile = UploadFile {
    _uploadFileFile          :: Entity DbFile
  , _uploadFileLocalFilePath :: FilePath
  } deriving (Eq, Show)

-- |
data PutRequest = PutRequest {
    _putBucket        :: BucketName -- ^ The bucket to store the file in.
  , _putFilename      :: ObjectKey  -- ^ The destination object key.
  , _putLocalFilePath :: FilePath   -- ^ The source file to upload.
  } deriving (Eq, Ord, Show)

-- |
putChunkedFile :: (MonadIO m) => PutRequest -> AppT' e m ()
putChunkedFile p@(PutRequest b k f) = do
    $(logDebug) "Uploading file to aws" ["putRequest" .= show p]
    runAwsAction $ do
        bdy <- chunkedFile defaultChunkSize f
        void . send $ putObject b k bdy

-- |
configBucket :: Monad m => AppT' e m BucketName
configBucket = BucketName . _awsConfigDemoBucketName <$> asks _configAwsEnv

-- |
runSend :: (AWSRequest r, Show r, MonadIO m) => r -> AppT' e m ()
runSend msg = do
    $(logDebug) "Sending request to aws" ["request" .= show msg]
    runAwsAction . void $ send msg

-- |
runAwsAction :: MonadIO m => AWST' Env (ResourceT IO) a -> AppT' e m a
runAwsAction m = do
    env <- _awsConfigEnv <$> asks _configAwsEnv
    liftIO $ runResourceT . runAWST env $ m

-- |
entityToFile :: Text -> Entity DbFile -> File
entityToFile s3BucketUrl (Entity k DbFile {..}) = File {
    fileId       = fileId
  , fileUserId   = userId
  , fileFolderId = fromSqlKey dbFileFolderId
  , fileName     = dbFileOriginalFileName
  , fileS3File   = s3File
  , fileS3Path   = s3BucketUrl <> s3File
  }
  where fileId = fromSqlKey k
        userId = fromSqlKey dbFileUserId
        s3File = tShow userId <> "/" <> tShow fileId <> "/" <> dbFileOriginalFileName
