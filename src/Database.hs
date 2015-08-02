{-# LANGUAGE OverloadedStrings #-}

module Database where

import Database.PostgreSQL.Simple
import Data.Text (Text(..), unpack)
import Data.ByteString.Char8 (pack)
import Control.Monad (forM_)
import Card
import System.Environment (lookupEnv)

getConnection = do
  (Just db_url) <- lookupEnv "DATABASE_URL"
  connectPostgreSQL (pack db_url)

migrate = do
  conn <- getConnection
  execute_ conn "CREATE TABLE IF NOT EXISTS card (title varchar(80), rules text, dominance int, cost int, cardType varchar(20), subType varchar(80), gene1 varchar(20), gene2 varchar(20));"
  
addFakeData = do
  conn <- getConnection
  execute_ conn "INSERT INTO card VALUES ('Xuukuu', 'Roam: +2', 1, 2, 'ting', 'animal', 'feather', 'small');"
  execute_ conn "INSERT INTO card VALUES ('Nice Blizzard', 'Crunch all seeds. Players gain $1 for each seed lost.', 0, 0, 'event', '', '', '');"

getCards :: IO [Card]
getCards = do
  conn <- getConnection
  cards <- query_ conn "SELECT title,rules,dominance,cost,cardType,subType,gene1,gene2 FROM card;"
  return $ map (\(t,r,d,c,ct,st,g1,g2) -> mkCard t r d c ct st g1 g2) cards

addCard :: Text -> Text -> IO ()
addCard title rules = do
  conn <- getConnection
  execute conn "INSERT INTO card VALUES (?, ?)" (title, rules)
  return ()
