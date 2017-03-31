module Util where

import Data.Text (pack, unpack, Text)

readText :: Read a => Text -> a
readText = read . unpack

showAsText :: Show a => a -> Text
showAsText = pack . show
