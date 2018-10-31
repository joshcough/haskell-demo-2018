
module Auth.Storage.UserStorage (
    UserDb(..)
  ) where

import           Control.Monad               (forM_)
import           Control.Monad.Except        (MonadIO, liftIO)
import           Crypto.BCrypt               (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import           Data.Text                   (Text)
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Database.Esqueleto
import qualified Database.Persist.Postgresql as P

import           Auth.DatabaseModels         (DbUser(..), DbUserId)
import qualified Auth.DatabaseModels         as Db
import           Auth.Models                 (CreateUser(..), User(..))
import           Config                      (AppT', runDb)

class Monad m => UserDb m where
    getUserById :: DbUserId -> m (Maybe User)
    getUserByUsername :: Text -> m (Maybe User)
    getUserByEmail :: Text -> m (Maybe (User, Text))
    deleteUserById :: DbUserId -> m ()
    createUser :: CreateUser -> m (Maybe DbUserId)
    -- TODO: this one is terrible.
    updateUserIfExists :: DbUserId -> User -> m ()

instance MonadIO m => UserDb (AppT' e m) where
    getUserById = runDb . getUserById
    getUserByUsername = runDb . getUserByUsername
    getUserByEmail = runDb . getUserByEmail
    deleteUserById = runDb . deleteUserById
    createUser = runDb . createUser
    updateUserIfExists uid = runDb . updateUserIfExists uid

instance MonadIO m => UserDb (SqlPersistT m) where
    getUserById = fmap (fmap entityToUser) . getEntity

    getUserByUsername username = fmap entityToUser <$> selectFirst [Db.DbUserName P.==. username] []

    getUserByEmail email = fmap f <$> selectFirst [Db.DbUserEmail P.==. email] []
        where f e@(Entity _ dbUser) = (entityToUser e, dbUserHashedPassword dbUser)

    deleteUserById = P.deleteCascade

    createUser (CreateUser name email pass) = do
        mPassword <- liftIO $ encryptPassword pass
        case mPassword of
            Nothing -> return Nothing -- TODO: need to throw an error here...
            Just password' -> Just <$> insert (DbUser name email $ decodeUtf8 password')
        where
        encryptPassword = hashPasswordUsingPolicy slowerBcryptHashingPolicy . encodeUtf8

    updateUserIfExists uid (User _ name email) = do
        maybeUser <- getEntity uid
        forM_ maybeUser $ \(Entity k v) -> replace k $ DbUser name email (dbUserHashedPassword v)

-- |
entityToUser :: Entity DbUser -> User
entityToUser (Entity k DbUser {..}) = User (fromSqlKey k) dbUserName dbUserEmail
