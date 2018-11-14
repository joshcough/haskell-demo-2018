
module FileServer.FileServerAPI (
    FileServerAPI
  , fileServer
  ) where

import           Control.Monad               (forM, void)
import           Control.Monad.Except        (MonadIO)
import           Database.Esqueleto          (Entity(..), fromSqlKey, toSqlKey)
import           ServantHelpers
import           Servant.Multipart           (FileData(..), MultipartData(..), MultipartForm, Tmp)

import           Auth.Models                 (User(..))
import           Config                      (AppT, runDb)
import           FileServer.DatabaseModels   (DbFileId, DbFile(..))
import           FileServer.Models           (File(..))
import qualified FileServer.S3               as S3
import qualified FileServer.Storage          as Db
import           Logging                     (logDebug)

type FileServerAPI = "file_server" :> Compose FileServer

data FileServer route = FileServer {
    fileAPIGetFileById :: route :- Capture "fileId" DbFileId :> Get '[JSON] File
  , fileAPIDeleteFile :: route :- Capture "fileId" DbFileId :> Delete '[JSON] ()
  , fileAPIUploadFile :: route :- MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] [File]
  } deriving Generic

-- | The server that runs the FileServerAPI
fileServer :: (MonadIO m) => User -> ServerT FileServerAPI (AppT m)
fileServer caller = toServant $ FileServer {
    fileAPIGetFileById = \fid -> withFileAccess fid S3.withS3Paths
  , fileAPIDeleteFile = \fid -> void . withFileAccess fid $ \dbFile -> do
        Db.deleteFile fid
        S3.deleteFile dbFile
  , fileAPIUploadFile = \multipartData -> do
        $(logDebug) "uploadFile" []
        forM (files multipartData) $ \fd -> do
             file' <- Db.insertFile (toSqlKey $ userId caller) (fdFileName fd)
             S3.uploadFile $ S3.UploadFile file' (fdPayload fd)
}
    where
    withFileAccess fid m = do
        mf <- runDb . Db.getFile $ fid
        maybeOr404 mf $ \f -> do
            tid <- pure . dbFileUserId . entityVal $  f
            guard401 (fromSqlKey tid == userId caller) (m f)

