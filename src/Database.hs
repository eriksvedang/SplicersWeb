{-# LANGUAGE OverloadedStrings #-}

module Database where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Control.Applicative ((<*>), (<$>))
import Data.Text (Text(..), unpack)
import Data.ByteString.Char8 (pack)
import Control.Monad (forM_)
import Card
import System.Environment (lookupEnv)
import Data.Text.Encoding

getConnection = do
  (Just db_url) <- lookupEnv "DATABASE_URL"
  connectPostgreSQL (pack db_url)

migrate = do
  conn <- getConnection
  execute_ conn "CREATE TABLE IF NOT EXISTS card (\
\ title varchar(80), \
\ rules text, \
\ dominance int, \
\ cost int, \
\ cardType varchar(20), \
\ subType varchar(80), \
\ gene1 varchar(20), \
\ gene2 varchar(20), \
\ startMatter int, \
\ startCards int, \
\ flavor text, \
\ designer varchar(80), \
\ key SERIAL PRIMARY KEY \
\);"
  
addFakeData = do
  conn <- getConnection
  execute_ conn "INSERT INTO card VALUES ('Xuukuu', 'Roam: +2', 1, 2, 'Ting', 'animal', 'Feather', 'Small', 0, 0, 'Xuuuuu!', 'Erik');"
  execute_ conn "INSERT INTO card VALUES ('Tulip', 'When tulip evolves, gain $1 for each close ting with [leaf]', 1, 1, 'Ting', 'plant', 'Leaf', 'Small', 0, 0, 'What a nice flower.', 'Erik');"
  execute_ conn "INSERT INTO card VALUES ('Nice Blizzard', 'Crunch all seeds. Players gain $1 for each seed lost.', 0, 0, 'Event', '', '', '', 0, 0, 'Damnit...', 'Erik');"
  execute_ conn "INSERT INTO card VALUES ('Ingvar Karlsson', 'When a friendly ting hunts, gain $1.', 0, 0, 'Splicer', 'politician', '', '', 0, 0, 'Warm and cozy', 'Erik');"
  execute_ conn "INSERT INTO card VALUES ('Djungle', 'Seeds enter play unexhausted here.', 0, 0, 'Biom', 'terran', '', '', 5, 3, 'Ruling with an iron fist', 'Erik');"
  execute_ conn "INSERT INTO card VALUES ('Crown', '+1', 0, 0, 'Mutation', '', '', '', 0, 0, 'You will be the queen', 'Erik');"

instance FromField Gene where
  fromField f bs = case bs of
    Just x -> return $ textToGene (decodeUtf8 x)
    Nothing -> error "Can't parse gene"

instance FromField CardType where
  fromField f bs = case bs of
    Just x -> return $ read (unpack (decodeUtf8 x))
    Nothing -> error "Can't parse card type"

instance FromRow Card where
  fromRow = Card <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Card where
  toRow card =
    [ toField (title card)
    , toField (rules card)
    , toField (dominance card)
    , toField (cost card)
    , toField (cardType card)
    , toField (subType card)
    , toField (gene1 card)
    , toField (gene2 card)
    , toField (startMatter card)
    , toField (startCards card)
    , toField (flavor card)
    , toField (designer card)
    ]

instance ToField Gene where
  toField gene = toField $ show gene

instance ToField CardType where
  toField cardType = toField $ show cardType

getCards :: IO [Card]
getCards = do
  conn <- getConnection
  cards <- query_ conn "SELECT title,rules,dominance,cost,cardType,subType,gene1,gene2,startMatter,startCards,flavor,designer FROM card;"
  return cards

addCard :: Card -> IO ()
addCard card = do
  conn <- getConnection
  execute conn "INSERT INTO card VALUES (?,?,?,?,?,?,?,?,?,?,?,?)" card
  return ()
