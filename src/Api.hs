
module Api (app) where

import           Conduit                  ( MonadThrow )
import           Control.Monad.Except     ( MonadIO, liftIO, throwError )
import           Servant
import           Servant.Auth.Server      hiding (throwAll)

import           Auth.Models              ( User )
import           Auth.UserAPI             ( LoginAPI, UserAPI, loginServer, userServer )
import           Config                   ( App, AppT, Config (..), runAppT )
import           Error                    ( AppError(..), AuthError(..), toServantErr, throwAll )
import           FileServer.FileServerAPI ( FileServerAPI, fileServer )

type Protected   = UserAPI :<|> FileServerAPI
type Unprotected = LoginAPI
type API         = (Auth '[Cookie, JWT] User :> Protected) :<|> Unprotected

app :: Config -> Application
app cfg = serveWithContext
            (Proxy :: Proxy (API :<|> Raw))
            (_configCookies cfg :. _configJWT cfg :. EmptyContext)
            (mainServer :<|> files)
    where
    convertApp :: Config -> App a -> Handler a
    convertApp cfg' appt = Handler $ do
        errOrResult <- liftIO $ runAppT appt cfg'
        either (throwError . toServantErr) return errOrResult

    mainServer :: Server API
    mainServer = hoistServerWithContext
        (Proxy :: Proxy API)
        (Proxy :: Proxy '[CookieSettings, JWTSettings])
        (convertApp cfg)
        (protected :<|> unprotected)

protected :: MonadIO m => AuthResult User -> ServerT Protected (AppT m)
protected (Authenticated u) = userServer u
                         :<|> fileServer u
protected _ = throwAll (AppAuthError NoAuthError)

unprotected :: (MonadThrow m, MonadIO m) => ServerT Unprotected (AppT m)
unprotected = loginServer

files :: Server Raw
files = serveDirectoryFileServer "frontend"
