
module Auth.LoginAPI where

import           Control.Monad.Except        (MonadError, MonadIO, liftIO)
import           Control.Monad.Reader        (MonadReader, asks)
import           Crypto.BCrypt               (validatePassword)
import           Data.Text.Encoding          (encodeUtf8)
import           ServantHelpers
import           Servant.Auth.Server

import           Auth.Models                 (Login(..), LoginSuccess(..), User(..))
import qualified Auth.UserStorage    as Db
import           Error                       (FileServerDemoError)
import           Config                      (AppT, Config(..), runDb)

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
