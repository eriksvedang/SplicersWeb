module Player where

import Data.Text (Text(..), unpack)
import Data.Monoid ((<>))

data Player = Player { playerName :: Text
                     , playerEmail :: Text
                     , playerPassword :: Text
                     , playerSalt :: Text
                     } deriving (Show)


