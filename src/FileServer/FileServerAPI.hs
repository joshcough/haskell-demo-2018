
module FileServer.FileServerAPI (
    FileServerAPI
  , fileServer
  ) where

import           Control.Monad               (forM, void)
import           Control.Monad.Except        (MonadIO, MonadError)
import           Database.Esqueleto          (Entity(..), fromSqlKey, toSqlKey)
import           ServantHelpers
import           Servant.Multipart           (FileData(..), MultipartData(..), MultipartForm, Tmp)

import           Auth.DatabaseModels         (DbUserId)
import           Auth.Models                 (User(..))
import           Config                      (AppT, runDb)
import           Error                       (FileServerDemoError)
import           FileServer.DatabaseModels   (DbFileId, DbFile(..))
import           FileServer.Models           (File(..))
import qualified FileServer.S3               as S3
import qualified FileServer.Storage          as Db
import           Logging                     (MonadLoggerJSON, logDebug)

type FileServerM m = (Accessible m DbFileId (Entity DbFile),
                      MonadError FileServerDemoError m,
                      S3.S3FileServer m,
                      Db.FilesDb m,
                      Monad m,
                      MonadLoggerJSON m)

type FileServerAPI = "file_server" :> Compose FileServer

data FileServer route = FileServer {
    fileServerFiles :: route :- "files" :> Compose FilesAPI
  } deriving Generic

data FilesAPI route = FilesAPI {
    fileAPIGetFileById :: route :- Capture "fileId" DbFileId :> Get '[JSON] File
  , fileAPIDeleteFile :: route :- Capture "fileId" DbFileId :> Delete '[JSON] ()
  , fileAPIUploadFile :: route :- MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] [File]
  } deriving Generic

-- | The server that runs the FileServerAPI
fileServer :: (MonadIO m) => User -> ServerT FileServerAPI (AppT m)
fileServer caller = toServant $ FileServer { fileServerFiles }
    where
    fileServerFiles = toServant FilesAPI {
        fileAPIGetFileById = getFile caller
      , fileAPIDeleteFile = deleteFile caller
      , fileAPIUploadFile = uploadFile caller
    }

-- |
getFile :: FileServerM m => User -> DbFileId -> m File
getFile caller fid = withAccess caller fid S3.withS3Paths

-- |
deleteFile :: FileServerM m => User -> DbFileId -> m ()
deleteFile caller fid = void . withAccess caller fid $ \dbFile -> do
    Db.deleteFile fid
    S3.deleteFile dbFile

-- |
uploadFile :: FileServerM m => User -> MultipartData Tmp -> m [File]
uploadFile caller multipartData = do
    $(logDebug) "uploadFile" []
    forM (files multipartData) $ \fd -> do
        file' <- Db.insertFile (toSqlKey $ userId caller) (fdFileName fd)
        S3.uploadFile $ S3.UploadFile file' (fdPayload fd)

-- |
class Monad m => Accessible m aId a | aId -> a, a -> aId where
    readFromDb :: aId -> m (Maybe a)
    getUserId :: a -> m DbUserId

-- |
instance MonadIO m => Accessible (AppT m) DbFileId (Entity DbFile) where
    readFromDb = runDb . Db.getFile
    getUserId = pure . dbFileUserId . entityVal

-- |
withAccess :: (MonadError FileServerDemoError m, Monad m, Accessible m fid f) => User -> fid -> (f -> m a) -> m a
withAccess caller fid m = do
    mf <- readFromDb fid
    maybeOr404 mf $ \f -> do
        tid <- getUserId f
        guard401 (fromSqlKey tid == userId caller) (m f)
