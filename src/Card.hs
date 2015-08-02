{-# LANGUAGE OverloadedStrings #-}

module Card where

import Data.Text (Text(..), unpack)

data Card = Card { title :: Text
                 , rules :: Text
                 , dominance :: Int
                 , cost :: Int
                 , cardType :: CardType
                 , subtype :: Text
                 , genes :: (Gene, Gene)
                 } deriving (Show)

data CardType = Ting
              | Event
              | Biom
              | Mutation
              | Identity
                deriving (Show)

data Gene = Sinister
          | Artificial
          | Nautic
          | Leaf
          | Bug
          | Land
          | Other Text
            deriving (Show)

mkTing title rules dominance cost =
  Card { title = title
       , rules = rules
       , dominance = dominance
       , cost = cost
       , cardType = Ting
       , subtype = "unknown"
       , genes = (Sinister, Nautic)
       }
