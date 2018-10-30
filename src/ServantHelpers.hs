
module ServantHelpers (
    module Servant
  , module Servant.Generic
  , Compose
  , toServant
  , adminOr401
  , guard401
  , maybeOr401
  , maybeOr404
  , maybeOr500
  , callerIsUserOr401
  , callerIsUserOrIsAdminElse401
  , unexpected
  ) where

import           Control.Monad.Except        (MonadError)
import           Database.Persist.Postgresql (fromSqlKey)
import           Data.Text                   (Text, unpack)
import           Servant
import           Servant.Generic      hiding (toServant)
import qualified Servant.Generic as S


import           Auth.DatabaseModels         (DbUserId)
import           Auth.Models                 (User(..))
import           Error                       (AppError(..), AuthError(..), FileServerDemoError, FileServerDemoError'(..))

---
--- servant generic helpers
---

type Compose api = S.ToServant (api S.AsApi)

toServant :: forall m api. S.GenericProduct (api (S.AsServerT m)) => api (S.AsServerT m) -> S.ToServant (api (S.AsServerT m))
toServant = S.toServant

---
--- error and access handling
---

unexpected :: MonadError FileServerDemoError m => Text -> m a
unexpected t = throwError . AppAppError . DemoMiscError $ unpack t

guard401 :: MonadError FileServerDemoError m => Bool -> m a -> m a
guard401 b m = if b then m else throwError $ AppNotFoundError ""

maybeOr404 :: MonadError FileServerDemoError m => Maybe a -> (a -> m b) -> m b
maybeOr404 = maybeOrErr $ AppNotFoundError ""

maybeOr401 :: MonadError FileServerDemoError m => Maybe a -> (a -> m b) -> m b
maybeOr401 = maybeOrErr (AppAuthError NoAuthError)

maybeOr500 :: MonadError FileServerDemoError m => Text -> Maybe a -> (a -> m b) -> m b
maybeOr500 msg = maybeOrErr (AppAppError $ DemoMiscError $ unpack msg)

maybeOrErr :: MonadError FileServerDemoError m => FileServerDemoError -> Maybe a -> (a -> m b) -> m b
maybeOrErr err = flip $ maybe (throwError err)

adminOr401 :: MonadError FileServerDemoError m => User -> m a -> m a
adminOr401 u m = if userEmail u == "joshcough@gmail.com" then m else throwError (AppAuthError NoAuthError)

callerIsUserOr401 :: MonadError FileServerDemoError m => User -> DbUserId -> m a -> m a
callerIsUserOr401 caller uid m = if userId caller == fromSqlKey uid then m else throwError (AppAuthError NoAuthError)

callerIsUserOrIsAdminElse401 :: MonadError FileServerDemoError m => User -> DbUserId -> m a -> m a
callerIsUserOrIsAdminElse401 caller uid m =
    if userEmail caller == "joshcough@gmail.com" || userId caller == fromSqlKey uid then m else throwError (AppAuthError NoAuthError)
