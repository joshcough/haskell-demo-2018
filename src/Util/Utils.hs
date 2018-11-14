
module Util.Utils (
    tShow
) where

import Data.Text (Text, pack)

-- |
tShow :: Show a => a -> Text
tShow = pack . show