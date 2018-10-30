
module Auth.Storage.UserStorage (
    createUser
  , deleteUserById
  , entityToUser
  , getUserByEmail
  , getUserById
  , getUserByUsername
  , updateUserIfExists
  ) where

import           Control.Monad.Except        (MonadIO, liftIO)
import           Crypto.BCrypt               (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import           Data.ByteString             (ByteString)
import           Data.Text                   (Text)
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Database.Esqueleto
import qualified Database.Persist.Postgresql as P

import           Auth.DatabaseModels         (DbUser(..), DbUserId)
import qualified Auth.DatabaseModels        as Db
import           Auth.Models                 (CreateUser(..), User(..))

-- |
getUserById :: MonadIO m => DbUserId -> SqlPersistT m (Maybe User)
getUserById = fmap (fmap entityToUser) . getEntity

-- |
getUserByUsername :: (MonadIO m) => Text -> SqlPersistT m (Maybe User)
getUserByUsername username = fmap entityToUser <$> selectFirst [Db.DbUserName P.==. username] []

-- |
getUserByEmail :: (MonadIO m) => Text -> SqlPersistT m (Maybe (User, Text))
getUserByEmail email = fmap f <$> selectFirst [Db.DbUserEmail P.==. email] []
    where f e@(Entity _ dbUser) = (entityToUser e, dbUserHashedPassword dbUser)

-- |
deleteUserById :: MonadIO m => DbUserId -> SqlPersistT m ()
deleteUserById = P.deleteCascade

-- |
createUser :: MonadIO m => CreateUser -> SqlPersistT m (Maybe DbUserId)
createUser (CreateUser name email pass) = do
    mPassword' <- liftIO $ encryptPassword pass
    case mPassword' of
        Nothing -> return Nothing
        Just password' -> do
            newUser <- insert $ DbUser name email (decodeUtf8 password')
            return . Just $ newUser
    where
    encryptPassword :: Text -> IO (Maybe ByteString)
    encryptPassword = hashPasswordUsingPolicy slowerBcryptHashingPolicy . encodeUtf8

-- |
updateUserIfExists ::  MonadIO m => DbUserId -> User -> SqlPersistT m ()
updateUserIfExists uid (User _ name email) = do
    maybeUser <- getEntity uid
    case maybeUser of
        Just (Entity k v) -> replace k $ DbUser name email (dbUserHashedPassword v)
        Nothing -> return ()

-- |
entityToUser :: Entity DbUser -> User
entityToUser (Entity k DbUser {..}) = User (fromSqlKey k) dbUserName dbUserEmail
