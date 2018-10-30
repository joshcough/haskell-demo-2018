
module FileServer.FileServerAPI (
    FileServerAPI
  , fileServer
  ) where

import           Control.Monad               (forM, forM_, void)
import           Control.Monad.Except        (MonadIO, MonadError)
import           Data.Aeson                  ((.=))
import           Data.Text                   (Text)
import           Database.Esqueleto          (Entity(..), fromSqlKey, toSqlKey)
import           ServantHelpers
import           Servant.Multipart           (FileData(..), MultipartData(..), MultipartForm, Tmp)

import           Auth.DatabaseModels         (DbUserId)
import           Auth.Models                 (User(..))
import           Config                      (AppT, runDb)
import           Error                       (FileServerDemoError)
import           FileServer.DatabaseModels   (DbFolderId, DbFileId, DbFile(..))
import           FileServer.Models           (File(..), Folder(..), MoveRequest(..))
import qualified FileServer.S3               as S3
import qualified FileServer.Storage          as Db
import           Logging                     (MonadLoggerJSON, logDebug)

type FileServerM m = (Accessible m DbFileId (Entity DbFile),
                      Accessible m DbFolderId (Folder (Entity DbFile)),
                      MonadError FileServerDemoError m,
                      S3.S3FileServer m,
                      Db.FilesDb m,
                      Monad m,
                      MonadLoggerJSON m)

type FileServerAPI = "file_server" :> Compose FileServer

data FileServer route = FileServer {
    fileServerFolders :: route :- "folders" :> Compose FoldersAPI
  , fileServerFiles :: route :- "files" :> Compose FilesAPI
  , fileServerMove :: route :- "move" :> ReqBody '[JSON] MoveRequest :> Post '[JSON] ()
  } deriving Generic

data FoldersAPI route = FoldersAPI {
    folderAPIGetFolderById :: route :- Capture "folderId" DbFolderId :> Get '[JSON] (Folder File)
  , folderAPICreateFolder :: route :- Capture "folderId" DbFolderId :> Capture "folderName" Text :> Post '[JSON] DbFolderId
  , folderAPIDelteFolder :: route :- Capture "folderId" DbFolderId :> Delete '[JSON] ()
  , folderAPIRootFolder :: route :- Get '[JSON] (Folder File)
  } deriving Generic

data FilesAPI route = FilesAPI {
    fileAPIGetFileById :: route :- Capture "fileId" DbFileId :> Get '[JSON] File
  , fileAPIDeleteFile :: route :- Capture "fileId" DbFileId :> Delete '[JSON] ()
  , fileAPIUploadFile :: route :- Capture "folderId" DbFolderId :>  MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] [File]
  } deriving Generic

-- | The server that runs the FileServerAPI
fileServer :: (MonadIO m) => User -> ServerT FileServerAPI (AppT m)
fileServer caller = toServant $ FileServer { fileServerFolders, fileServerFiles, fileServerMove }
    where
    fileServerFolders = toServant FoldersAPI {
        folderAPIGetFolderById = getFolder caller
      , folderAPICreateFolder = insertFolder caller
      , folderAPIDelteFolder = deleteFolder caller
      , folderAPIRootFolder = getRootFolder caller
    }
    fileServerFiles = toServant FilesAPI {
        fileAPIGetFileById = getFile caller
      , fileAPIDeleteFile = deleteFile caller
      , fileAPIUploadFile = uploadFile caller
    }
    fileServerMove = move caller

-- |
getRootFolder :: FileServerM m => User -> m (Folder File)
getRootFolder caller = do
    mf <- Db.getRootFolder (toSqlKey $ userId caller)
    maybeOr500 "Unable to locate root folder" mf (mapM S3.withS3Paths)

-- |
getFolder :: FileServerM m => User -> DbFolderId -> m (Folder File)
getFolder caller fid = withAccess caller fid (mapM S3.withS3Paths)

-- |
getFile :: FileServerM m => User -> DbFileId -> m File
getFile caller fid = withAccess caller fid S3.withS3Paths

-- |
deleteFolder :: FileServerM m => User -> DbFolderId -> m ()
deleteFolder caller fid = void . withAccess caller fid . const $ do
    mFolder <- Db.getFolderRecursive fid
    maybeOr404 mFolder $ \folder -> do
        Db.deleteFolder fid
        S3.deleteFolder folder

-- |
deleteFile :: FileServerM m => User -> DbFileId -> m ()
deleteFile caller fid = void . withAccess caller fid $ \dbFile -> do
    Db.deleteFile fid
    S3.deleteFile dbFile

-- |
insertFolder :: FileServerM m => User -> DbFolderId -> Text -> m DbFolderId
insertFolder caller = Db.insertFolder (toSqlKey $ userId caller)

-- |
uploadFile :: FileServerM m => User -> DbFolderId -> MultipartData Tmp -> m [File]
uploadFile caller fid multipartData = do
    $(logDebug) "uploadFile" ["fid" .= fid]
    withAccess caller fid $ \_ ->
        forM (files multipartData) $ \fd -> do
            file' <- Db.insertFile (toSqlKey $ userId caller) fid (fdFileName fd)
            S3.uploadFile $ S3.UploadFile file' (fdPayload fd)

-- |
move :: FileServerM m => User -> MoveRequest -> m ()
move caller MoveRequest{..} = move' caller (toSqlKey moveTo) (toSqlKey <$> moveFiles) (toSqlKey <$> moveFolders)

-- | TODO: refactor this so that all the moves are in one db transaction.
-- | TODO: must insert checks that folder isn't itself or a child of itself! otherwise code will infinite loop
move' :: FileServerM m => User -> DbFolderId -> [DbFileId] -> [DbFolderId] -> m ()
move' caller moveTo moveFiles moveFolders = do
    $(logDebug) "uploadFile" []
    withAccess caller moveTo $ \moveTo' -> do
        forM_ moveFiles   $ \fid -> withAccess caller fid $ \(Entity k _) -> Db.moveFile   k      moveTo'
        forM_ moveFolders $ \fid -> withAccess caller fid $ \folder       -> Db.moveFolder folder moveTo'

-- |
class Monad m => Accessible m aId a | aId -> a, a -> aId where
    readFromDb :: aId -> m (Maybe a)
    getUserId :: a -> m DbUserId

-- |
instance MonadIO m => Accessible (AppT m) DbFolderId (Folder (Entity DbFile)) where
    readFromDb = runDb . Db.getFolder
    getUserId = pure . toSqlKey . folderUserId

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
