module Keyword where

import Data.Text (Text)

data Keyword = Keyword { keywordName :: Text
                       , keywordRules :: Text
                       } deriving (Show)
