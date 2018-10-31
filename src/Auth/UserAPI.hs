
module Auth.UserAPI where

import           Control.Monad.Except        (MonadError, MonadIO, liftIO)
import           Control.Monad.Reader        (MonadReader, asks)
import           Crypto.BCrypt               (validatePassword)
import           Data.Aeson                  ((.=))
import           Data.Text.Encoding          (encodeUtf8)
import           ServantHelpers
import           Servant.Auth.Server

import           Auth.DatabaseModels         (DbUserId)
import           Auth.Models                 (CreateUser(..), Login(..), LoginSuccess(..), User(..))
import qualified Auth.Storage.UserStorage    as Db
import           Error                       (FileServerDemoError)
import           Config                      (AppT, Config(..), runDb)
import           Logging                     (logDebug, logDebug_)

type SetCookieHeader  = Header "Set-Cookie" SetCookie
type SetCookieHeaders = '[SetCookieHeader, SetCookieHeader]

---
--- Login API/Server
---

type LoginAPI = "login" :> Compose LoginServer

newtype LoginServer route = LoginServer {
    loginServerLogin :: route :-  ReqBody '[JSON] Login :> Post '[JSON] (Headers SetCookieHeaders LoginSuccess)
  } deriving Generic

loginServer :: MonadIO m => ServerT LoginAPI (AppT m)
loginServer = toServant $ LoginServer login

{-
 - Here is the login handler. We do the following:
 - A) look up the user in the database by email addr, and throw 404 if not found
 - B) Check to see if they entered a valid password, and throw a 401 if not
 - C) Return the jwt token in the header.
 -}
login :: MonadIO m => Login -> AppT m (Headers SetCookieHeaders LoginSuccess)
login (Login e pw) = do
    maybeUT <- runDb $ Db.getUserByEmail e
    maybeOr401 maybeUT $ \(user, hashedPw) -> guard401 (validate hashedPw) (applyCookies user)
    where
    validate hashedPw = validatePassword (encodeUtf8 hashedPw) (encodeUtf8 pw)

    applyCookies :: (MonadError FileServerDemoError m, MonadIO m, MonadReader Config m) =>
        User -> m (Headers SetCookieHeaders LoginSuccess)
    applyCookies usr = do
        cookieSettings <- asks _configCookies
        jwtSettings    <- asks _configJWT
        mApplyCookies  <- liftIO $ acceptLogin cookieSettings jwtSettings usr
        maybeOr401 mApplyCookies (\app -> return . app $ LoginSuccess usr)

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
    userServerGetUserById = getUserById caller
    userServerDeleteUser = deleteUser caller
    userServerCreateUser = createUser caller
    userServerUpdateUser = updateUser caller

-- | Returns a user by name or throws a 404 error.
getUserById :: MonadIO m => User -> DbUserId -> AppT m User
getUserById caller uid = do
    $(logDebug) "getting user" ["uid" .= uid]
    callerIsUserOrIsAdminElse401 caller uid $ withUserOr404 uid return

-- | Creates a user in the database.
deleteUser :: MonadIO m => User -> DbUserId -> AppT m ()
deleteUser caller uid = do
    $(logDebug) "deleting user" ["uid" .= uid]
    adminOr401 caller $ withUserOr404 uid (const . runDb $ Db.deleteUserById uid)

-- | Creates a user in the database.
createUser :: MonadIO m => User -> CreateUser -> AppT m DbUserId
createUser caller c = do
    $(logDebug_) "creating user"
    adminOr401 caller $ runDb (Db.createUser c) >>= flip (maybeOr500 "Couldn't create user.") return

-- | Update a user in the database.
updateUser :: MonadIO m => User -> DbUserId -> User -> AppT m ()
updateUser caller uid u = do
    $(logDebug) "updating user" ["uid" .= uid, "user" .= u]
    callerIsUserOrIsAdminElse401 caller uid $ withUserOr404 uid . const . runDb $ Db.updateUserIfExists uid u

-- | Look up a user by id. If it exist, run an operation on it. If not, throw a 404.
withUserOr404 :: (MonadError FileServerDemoError m, MonadIO m, MonadReader Config m) => DbUserId -> (User -> m b) -> m b
withUserOr404 uid m = runDb (Db.getUserById uid) >>= flip maybeOr404 m
