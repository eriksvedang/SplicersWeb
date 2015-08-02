{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Card where

import Data.Text (Text(..), unpack)
import Data.Monoid ((<>))

data Card = Card { title :: Text
                 , rules :: Text
                 , dominance :: Int
                 , cost :: Int
                 , cardType :: CardType
                 , subType :: Text
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
          | Feather
          | Other Text
          | NoGene
            deriving (Show)

mkCard ::  Text -> Text -> Int -> Int -> Text -> Text -> Text -> Text -> Card
mkCard title rules dominance cost cardType subType gene1 gene2 =
  case cardType of
  "ting" -> mkTing title rules dominance cost subType gene1 gene2
  "event" -> mkEvent title rules dominance subType
  _ -> error $ unpack ("Unknown card type '" <> cardType <> "'") 

textToGene geneText =
  case geneText of
  "artificial" -> Artificial
  "nautic" -> Nautic
  "leaf" -> Leaf
  "bug" -> Bug
  "land" -> Land
  "feather" -> Feather
  x -> Other x

mkTing title rules dominance cost subType gene1 gene2 =
  Card { title = title
       , rules = rules
       , dominance = dominance
       , cost = cost
       , cardType = Ting
       , subType = subType
       , genes = (textToGene gene1, textToGene gene2)
       }

mkEvent title rules dominance subType =
  Card { title = title
       , rules = rules
       , dominance = dominance
       , cost = 0
       , cardType = Event
       , subType = subType
       , genes = (NoGene, NoGene)
       }
