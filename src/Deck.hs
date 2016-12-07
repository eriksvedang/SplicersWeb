module Deck where

import Data.Text (Text(..), unpack)
import Data.Monoid ((<>))
import Player

data Deck = Deck { deckId :: Int
                 , deckName :: Text
                 , deckDesigner :: Text
                 } deriving (Show)



