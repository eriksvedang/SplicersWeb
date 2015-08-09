{-# LANGUAGE OverloadedStrings #-}

module Database where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Internal (Connection)
import Control.Applicative ((<*>), (<$>))
import Data.Text (Text(..), unpack)
import Data.ByteString.Char8 (pack)
import Control.Monad (forM_)
import Card
import System.Environment (lookupEnv)
import Data.Text.Encoding

getConnection :: IO Connection
getConnection = do
  maybeDbUrl <- lookupEnv "DATABASE_URL"
  dbUrl <- case maybeDbUrl of
    (Just dbUrl) -> return dbUrl
    Nothing -> do
      putStrLn "No DATABASE_URL string found in environment, using default one."
      return "dbname=splicers user=erik"
  connectPostgreSQL (pack dbUrl)

migrate :: IO ()
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
\ illustration text, \
\ key SERIAL PRIMARY KEY \
\);"
  cards <- getCards
  if (length cards) == 0 then addFakeData else return ()

addFakeData :: IO ()
addFakeData = do
  conn <- getConnection
  execute_ conn "INSERT INTO card VALUES ('Xuukuu', 'Roam: +1', 1, 2, 'Ting', 'animal', 'Feather', 'Small', 0, 0, 'Xuuuuu!', 'Erik', 'https://c1.staticflickr.com/1/85/209708058_b5a5fb07a6_z.jpg?zz=1');"
  execute_ conn "INSERT INTO card VALUES ('Tulip', 'When tulip evolves, gain $1 for each close ting with [leaf]', 1, 1, 'Ting', 'plant', 'Leaf', 'Small', 0, 0, 'What a nice flower.', 'Erik', 'https://c1.staticflickr.com/1/85/209708058_b5a5fb07a6_z.jpg?zz=1');"
  execute_ conn "INSERT INTO card VALUES ('Nice Blizzard', 'Crunch all seeds. Players gain $1 for each seed lost.', 0, 0, 'Event', '', '', '', 0, 0, 'Damnit...', 'Erik', 'https://c1.staticflickr.com/1/85/209708058_b5a5fb07a6_z.jpg?zz=1');"
  execute_ conn "INSERT INTO card VALUES ('Ingvar Karlsson', 'When a friendly ting hunts, gain $1.', 0, 0, 'Splicer', 'politician', '', '', 0, 0, 'Warm and cozy', 'Erik', 'https://c1.staticflickr.com/1/85/209708058_b5a5fb07a6_z.jpg?zz=1');"
  execute_ conn "INSERT INTO card VALUES ('Djungle', 'Seeds enter play unexhausted here.', 0, 0, 'Biom', 'terran', '', '', 5, 3, 'Ruling with an iron fist', 'Erik', 'https://c1.staticflickr.com/1/85/209708058_b5a5fb07a6_z.jpg?zz=1');"
  execute_ conn "INSERT INTO card VALUES ('Crown', '+1', 0, 0, 'Mutation', '', '', '', 0, 0, 'You will be the queen', 'Erik', 'https://c1.staticflickr.com/1/85/209708058_b5a5fb07a6_z.jpg?zz=1');"
  execute_ conn "INSERT INTO card VALUES ('Xuukuu', 'Roam: +2', 1, 2, 'Ting', 'animal', 'Feather', 'Small', 0, 0, 'Better Xuuuuu!', 'Erik', 'https://c1.staticflickr.com/1/85/209708058_b5a5fb07a6_z.jpg?zz=1');"
  execute_ conn "INSERT INTO card VALUES ('Xuukuu', 'Roam: +2', 1, 2, 'Ting', 'animal', 'Feather', 'Small', 0, 0, 'Final Xuuuuu?!', 'Erik', 'https://pbs.twimg.com/profile_images/80734130/blbw.jpg');"
  return ()

instance FromRow Card where
  fromRow = Card <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromField Gene where
  fromField f bs = case bs of
    Just x -> return $ textToGene (decodeUtf8 x)
    Nothing -> error "Can't parse gene"

instance FromField CardType where
  fromField f bs = case bs of
    Just x -> return $ read (unpack (decodeUtf8 x))
    Nothing -> error "Can't parse card type"

getCards :: IO [Card]
getCards = do
  conn <- getConnection
  cards <- query_ conn "SELECT DISTINCT ON (title) title, rules, dominance, cost, cardType, subType, gene1, gene2, startMatter, startCards, flavor, designer, illustration FROM card;"
  return cards

getCardsWithTitle :: Text -> IO [Card]
getCardsWithTitle title = do
  conn <- getConnection
  cards <- query conn "SELECT title, rules, dominance, cost, cardType, subType, gene1, gene2, startMatter, startCards, flavor, designer, illustration FROM card WHERE title = ? ORDER BY key DESC;" (Only title)
  return cards

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
    , toField (illustration card)
    ]

instance ToField Gene where
  toField gene = toField $ show gene

instance ToField CardType where
  toField cardType = toField $ show cardType

addCard :: Card -> IO ()
addCard card = do
  conn <- getConnection
  execute conn "INSERT INTO card VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" card
  return ()

authorize :: Text -> Text -> IO (Maybe Text)
authorize username password = do
  return $ case (username, password) of
    ("Erik", "kaka") -> Just "kanelbulle"
    ("Ossian", "hihi") -> Just "kanelbulle"
    _ -> Nothing

getSecret :: Text -> IO (Maybe Text)
getSecret username = do
  return $ case username of
    "Erik" -> Just "kanelbulle"
    "Ossian" -> Just "kanelbulle"
    _ -> Nothing
