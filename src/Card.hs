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
                 , gene1 :: Gene
                 , gene2 :: Gene
                 , startMatter :: Int
                 , startCards :: Int
                 , flavor :: Text
                 , designer :: Text
                 } deriving (Show)

data CardType = Ting
              | Event
              | Biom
              | Mutation
              | Splicer
                deriving (Show, Read)

data Gene = Sinister
          | Artificial
          | Nautic
          | Leaf
          | Bug
          | Land
          | Feather
          | Small
          | Other Text
          | NoGene
            deriving (Show)

mkCard ::  Text -> Text -> Int -> Int -> Text -> Text -> Text -> Text -> Int -> Int -> Text -> Text -> Card
mkCard title rules dominance cost cardType subType gene1 gene2 startMatter startCards flavor designer =
  case cardType of
  "Ting" -> mkTing title rules dominance cost subType gene1 gene2 flavor designer
  "Event" -> mkEvent title rules subType flavor designer
  "Biom" -> mkBiom title rules dominance subType flavor designer
  "Mutation" -> mkMutation title rules subType flavor designer
  "Splicer" -> mkSplicer title rules subType startMatter startCards flavor designer
  _ -> error $ unpack ("Unknown card type '" <> cardType <> "'") 

textToGene :: Text -> Gene
textToGene geneText =
  case geneText of
  "Artificial" -> Artificial
  "Nautic" -> Nautic
  "Leaf" -> Leaf
  "Bug" -> Bug
  "Land" -> Land
  "Feather" -> Feather
  "Small" -> Small
  x -> Other x

mkTing title rules dominance cost subType gene1 gene2 flavor designer =
  Card { title = title
       , rules = rules
       , dominance = dominance
       , cost = cost
       , cardType = Ting
       , subType = subType
       , gene1 = textToGene gene1
       , gene2 = textToGene gene2
       , startMatter = 0
       , startCards = 0
       , flavor = flavor
       , designer = designer
       }

mkEvent title rules subType flavor designer =
  Card { title = title
       , rules = rules
       , dominance = 0
       , cost = 0
       , cardType = Event
       , subType = subType
       , gene1 = NoGene
       , gene2 = NoGene
       , startMatter = 0
       , startCards = 0
       , flavor = flavor
       , designer = designer
       }

mkBiom title rules domination subType flavor designer =
  Card { title = title
       , rules = rules
       , dominance = 0
       , cost = 0
       , cardType = Biom
       , subType = subType
       , gene1 = NoGene
       , gene2 = NoGene
       , startMatter = 0
       , startCards = 0
       , flavor = flavor
       , designer = designer                 
       }

mkMutation title rules subType flavor designer =
  Card { title = title
       , rules = rules
       , dominance = 0
       , cost = 0
       , cardType = Mutation
       , subType = subType
       , gene1 = NoGene
       , gene2 = NoGene
       , startMatter = 0
       , startCards = 0
       , flavor = flavor
       , designer = designer
       }

mkSplicer title rules subType startMatter startCards flavor designer =
  Card { title = title
       , rules = rules
       , dominance = 0
       , cost = 0
       , cardType = Splicer
       , subType = subType
       , gene1 = NoGene
       , gene2 = NoGene
       , startMatter = startMatter
       , startCards = startCards
       , flavor = flavor
       , designer = designer
       }
