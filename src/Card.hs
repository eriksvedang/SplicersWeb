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
              | Splicer
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
  "event" -> mkEvent title rules subType
  "biom" -> mkBiom title rules dominance subType
  "mutation" -> mkMutation title rules subType
  "splicer" -> mkSplicer title rules subType
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

mkEvent title rules subType =
  Card { title = title
       , rules = rules
       , dominance = 0
       , cost = 0
       , cardType = Event
       , subType = subType
       , genes = (NoGene, NoGene)
       }

mkBiom title rules domination subType =
  Card { title = title
       , rules = rules
       , dominance = 0
       , cost = 0
       , cardType = Biom
       , subType = subType
       , genes = (NoGene, NoGene)
       }

mkMutation title rules subType =
  Card { title = title
       , rules = rules
       , dominance = 0
       , cost = 0
       , cardType = Mutation
       , subType = subType
       , genes = (NoGene, NoGene)
       }

mkSplicer title rules subType =
  Card { title = title
       , rules = rules
       , dominance = 0
       , cost = 0
       , cardType = Splicer
       , subType = subType
       , genes = (NoGene, NoGene)
       }
