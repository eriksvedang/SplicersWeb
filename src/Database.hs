{-# LANGUAGE OverloadedStrings #-}

module Database where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Internal (Connection)
import Control.Applicative ((<*>), (<$>))
import Data.Text (Text(..), pack, unpack)
import qualified Data.ByteString.Char8 as BS
import Control.Monad (forM_)
import System.Environment (lookupEnv)
import Data.Text.Encoding

import Card
import Player
import Keyword
import Deck

getConnection :: IO Connection
getConnection = do
  maybeDbUrl <- lookupEnv "DATABASE_URL"
  dbUrl <- case maybeDbUrl of
    (Just dbUrl) -> return dbUrl
    Nothing -> do
      putStrLn "No DATABASE_URL string found in environment, using default one."
      return "dbname=splicers user=erik"
  connectPostgreSQL (BS.pack dbUrl)

migrate :: IO ()
migrate = do
  conn <- getConnection
  execute_ conn "CREATE TABLE IF NOT EXISTS card (\
               \ title varchar(80), \
               \ rules text, \
               \ dominance int, \
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

  execute_ conn "CREATE TABLE IF NOT EXISTS player (name VARCHAR(128) PRIMARY KEY, email VARCHAR(256), password VARCHAR(128));"

  players <- getPlayers
  if (length players) == 0 then addAdminPlayers else return ()

  execute_ conn "CREATE TABLE IF NOT EXISTS keyword (name VARCHAR(128) PRIMARY KEY, rules text);"
  keywords <- getKeywords
  if (length keywords) == 0 then addDefaultKeywords else return ()

  execute_ conn "CREATE TABLE IF NOT EXISTS deck (id SERIAL PRIMARY KEY, name VARCHAR(128), designer VARCHAR(128) REFERENCES player);"

  execute_ conn "CREATE TABLE IF NOT EXISTS inDeck (id SERIAL PRIMARY KEY, deck integer REFERENCES deck, cardTitle varchar(80));"
  
  return ()

addFakeData :: IO ()
addFakeData = do
  conn <- getConnection
  execute_ conn "INSERT INTO card VALUES ('Xuukuu', 'Roam: +1', 1, 'Ting', 'animal', 'Feather', 'Small', 0, 0, 'Xuuuuu!', 'Erik', 'https://c1.staticflickr.com/1/85/209708058_b5a5fb07a6_z.jpg?zz=1');"
  execute_ conn "INSERT INTO card VALUES ('Tulip', 'When tulip evolves, gain $1 for each close ting with [leaf]', 1, 'Ting', 'plant', 'Leaf', 'Small', 0, 0, 'What a nice flower.', 'Erik', '');"
  execute_ conn "INSERT INTO card VALUES ('Nice Blizzard', 'Crunch all seeds. Players gain $1 for each seed lost.', 0, 'Event', '', '', '', 0, 0, 'Damnit...', 'Erik', '');"
  execute_ conn "INSERT INTO card VALUES ('Ingvar Karlsson', 'When a friendly ting hunts, gain $1.', 0, 'Splicer', 'politician', '', '', 0, 0, 'Warm and cozy', 'Erik', '');"
  execute_ conn "INSERT INTO card VALUES ('Djungle', 'Seeds enter play unexhausted here.', 0, 'Biom', 'terran', '', '', 5, 3, 'Ruling with an iron fist', 'Erik', 'https://c1.staticflickr.com/7/6025/5938256884_cd593b60f8_b.jpg');"
  execute_ conn "INSERT INTO card VALUES ('Crown', '+1', 0, 'Mutation', '', '', '', 0, 0, 'You will be the queen', 'Erik', '');"
  execute_ conn "INSERT INTO card VALUES ('Xuukuu', 'Roam: +2', 1, 'Ting', 'animal', 'Feather', 'Small', 0, 0, 'Better Xuuuuu!', 'Erik', 'https://c1.staticflickr.com/1/85/209708058_b5a5fb07a6_z.jpg?zz=1');"
  execute_ conn "INSERT INTO card VALUES ('Xuukuu', 'Roam: +2', 1, 'Ting', 'animal', 'Feather', 'Small', 0, 0, 'Final Xuuuuu?!', 'Erik', 'https://pbs.twimg.com/profile_images/80734130/blbw.jpg');"
  return ()

instance FromRow Card where
  fromRow = Card <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

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
  cards <- query_ conn "SELECT DISTINCT ON (title) title, rules, dominance, cardType, subType, gene1, gene2, startMatter, startCards, flavor, designer, illustration FROM card ORDER BY title, key desc;"
  return cards

getCardsWithTitle :: Text -> IO [Card]
getCardsWithTitle title = do
  conn <- getConnection
  cards <- query conn "SELECT title, rules, dominance, cardType, subType, gene1, gene2, startMatter, startCards, flavor, designer, illustration FROM card WHERE title = ? ORDER BY key DESC;" (Only title)
  return cards

getCardsByDesigner :: Text -> IO [Card]
getCardsByDesigner designer = do
  conn <- getConnection
  cards <- query conn "SELECT title, rules, dominance, cardType, subType, gene1, gene2, startMatter, startCards, flavor, designer, illustration FROM card WHERE designer = ? ORDER BY key DESC;" (Only designer)
  return cards

instance ToRow Card where
  toRow card =
    [ toField (title card)
    , toField (rules card)
    , toField (dominance card)
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
  execute conn "INSERT INTO card VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" card
  return ()

authorize :: Text -> Text -> IO (Maybe Text)
authorize username password = do
  player <- getPlayer username
  case player of
    Just player -> if playerPassword player == password then
                     return $ Just "kanelbulle"
                   else
                     return Nothing
    Nothing -> return Nothing

getSecret :: Text -> IO (Maybe Text)
getSecret name = do
  p <- getPlayer name
  return $ case p of
    Just player -> Just "pepparkaka"
    Nothing -> Nothing

-- Player

instance FromRow Player where
  fromRow = Player <$> field <*> field <*> field

instance ToRow Player where
  toRow player = [ toField (playerName player)
                 , toField (playerEmail player)
                 , toField (playerPassword player) ]

getPlayers :: IO [Player]
getPlayers = do
  conn <- getConnection
  players <- query_ conn "SELECT name, email, password FROM player;"
  return players

getPlayer :: Text -> IO (Maybe Player)
getPlayer name = do
  conn <- getConnection
  players <- query conn "SELECT name, email, password FROM player WHERE name = ?;" (Only name)
  if length players > 0 then return $ Just (head players)
  else return Nothing

addPlayer :: Player -> IO Bool
addPlayer player = do
  existing <- getPlayer (playerName player)
  case existing of
    Just x -> return False
    Nothing -> do conn <- getConnection
                  execute conn "INSERT INTO player VALUES (?, ?, ?)" player
                  return True

addAdminPlayers :: IO ()
addAdminPlayers = do
  addPlayer (Player "erik" "erik.svedang@gmail.com" "kaka")
  addPlayer (Player "catnipped" "ossianboren@gmail.com" "hihi")
  return ()


-- Keyword

instance FromRow Keyword where
  fromRow = Keyword <$> field <*> field

instance ToRow Keyword where
  toRow keyword = [ toField (keywordName keyword)
                  , toField (keywordRules keyword) ]

getKeywords :: IO [Keyword]
getKeywords = do
  conn <- getConnection
  keywords <- query_ conn "SELECT name, rules FROM keyword;"
  return keywords

addKeyword :: Keyword -> IO ()
addKeyword keyword = do
  conn <- getConnection
  execute conn "INSERT INTO keyword VALUES (?, ?)" keyword
  return ()

addDefaultKeywords = do
  addKeyword (Keyword "Grace" "Requires nearby plant. Get 1 action.")
  addKeyword (Keyword "Hunt" "Devolve a nearby creature.")


-- Deck

instance FromRow Deck where
  fromRow = Deck <$> field <*> field <*> field

instance ToRow Deck where
  -- The id of 'deck' is not stored since it's automatically generated in the DB.
  toRow deck = [ toField (deckName deck)
               , toField (deckDesigner deck)]

deck1 = Deck 0 "blurg" "erik"

addDeck :: Deck -> IO ()
addDeck deck = do
  conn <- getConnection
  execute conn "INSERT INTO deck (name, designer) VALUES (?, ?);" deck
  return ()

getDecks :: Text -> IO [Deck]
getDecks username = do
  conn <- getConnection
  decks <- query conn "SELECT id, name, designer FROM deck WHERE designer=?" (Only username)
  return decks

getDeck :: Int -> IO Deck
getDeck deckId = do
  conn <- getConnection
  decks <- query conn "SELECT id, name, designer FROM deck WHERE id=?" (Only deckId)
  case decks of
    [deck] -> return deck
    _ -> error "Failed to get deck."

-- Add cards to Deck
instance FromRow InDeck where
  fromRow = InDeck <$> field <*> field <*> field

instance ToRow InDeck where
  -- ignore 'id' field, just like with the Deck
  toRow inDeck = [ toField (inDeckDeck inDeck)
                 , toField (inDeckCardTitle inDeck)]
               
addCardToDeck :: Int -> Text -> IO ()
addCardToDeck deckId cardTitle = do
  conn <- getConnection
  execute conn "INSERT INTO inDeck (deck, cardTitle) VALUES (?, ?);" (InDeck 0 deckId cardTitle)
  return ()

instance FromRow Text where
  fromRow = pack <$> field

getCardsInDeck :: Int -> IO [Text]
getCardsInDeck deckId = do
  conn <- getConnection
  cards <- query conn "SELECT cardTitle FROM inDeck WHERE inDeck.deck=?" (Only deckId)
  return cards
