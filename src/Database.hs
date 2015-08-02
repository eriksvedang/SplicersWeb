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
  execute_ conn "CREATE TABLE IF NOT EXISTS card (title varchar(80), rules text, dominance int, cost int);"
  
addFakeData = do
  conn <- getConnection
  execute_ conn "INSERT INTO card VALUES ('Xuukuu', 'Roam: +2', 1, 2);"
  execute_ conn "INSERT INTO card VALUES ('Nice Blizzard', 'Crunch all seeds. Players gain $1 for each seed lost.', 0, 0);"

getCards :: IO [Card]
getCards = do
  conn <- getConnection
  cards <- query_ conn "SELECT title,rules,dominance,cost FROM card;"
  return $ map (\(t,r,d,c) -> mkTing t r d c) cards

addCard :: Text -> Text -> IO ()
addCard title rules = do
  conn <- getConnection
  execute conn "INSERT INTO card VALUES (?, ?)" (title, rules)
  return ()
