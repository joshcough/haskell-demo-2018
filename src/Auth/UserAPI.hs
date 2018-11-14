
module Auth.UserAPI where

import           Control.Monad.Except        (MonadError, MonadIO)
import           Control.Monad.Reader        (MonadReader)
import           Data.Aeson                  ((.=))
import           ServantHelpers

import           Auth.DatabaseModels         (DbUserId)
import           Auth.Models                 (CreateUser(..), User(..))
import qualified Auth.UserStorage    as Db
import           Error                       (FileServerDemoError)
import           Config                      (AppT, Config(..), runDb)
import           Logging                     (logDebug, logDebug_)

---
--- User API/Server
---

type UserAPI = "users" :> Compose UserServer

data UserServer route = UserServer {
    userServerGetUserById :: route :- Capture "id" DbUserId :> Get '[JSON] User
  , userServerDeleteUser :: route :- Capture "id" DbUserId :> Delete '[JSON] ()
  , userServerCreateUser :: route :- ReqBody '[JSON] CreateUser :> Post '[JSON] DbUserId
  , userServerUpdateUser :: route :- Capture "id" DbUserId :> ReqBody '[JSON] User :> Put '[JSON] ()
  } deriving Generic

-- | The server that runs the UserAPI
userServer :: (MonadIO m) => User -> ServerT UserAPI (AppT m)
userServer caller = toServant $ UserServer { .. }
    where
    userServerGetUserById uid = do
        $(logDebug) "getting user" ["uid" .= uid]
        callerIsUserOrIsAdminElse401 caller uid $ withUserOr404 uid return
    userServerDeleteUser uid = do
        $(logDebug) "deleting user" ["uid" .= uid]
        adminOr401 caller $ withUserOr404 uid (const . runDb $ Db.deleteUserById uid)
    userServerCreateUser c =  do
        $(logDebug_) "creating user"
        adminOr401 caller $ runDb (Db.createUser c) >>= flip (maybeOr500 "Couldn't create user.") return
    userServerUpdateUser uid u = do
        $(logDebug) "updating user" ["uid" .= uid, "user" .= u]
        callerIsUserOrIsAdminElse401 caller uid $ withUserOr404 uid . const . runDb $ Db.updateUserIfExists uid u

-- | Look up a user by id. If it exist, run an operation on it. If not, throw a 404.
withUserOr404 :: (MonadError FileServerDemoError m, MonadIO m, MonadReader Config m) => DbUserId -> (User -> m b) -> m b
withUserOr404 uid m = runDb (Db.getUserById uid) >>= flip maybeOr404 m
