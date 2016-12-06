module Keyword where

import Data.Text (Text(..), unpack)
import Data.Monoid ((<>))

data Keyword = Keyword { keywordName :: Text
                       , keywordRules :: Text
                       } deriving (Show)

