module Player where

import Data.Text (Text)

data Player = Player { playerName :: Text
                     , playerEmail :: Text
                     , playerPassword :: Text
                     , playerSalt :: Text
                     } deriving (Show)
