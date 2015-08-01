module Card where

import Data.Text (Text(..), unpack)

data Card = Card { title :: Text
                 , rules :: Text
                 , dominance :: Int
                 , cost :: Int
                 } deriving (Show)

