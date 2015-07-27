{-# LANGUAGE OverloadedStrings #-}

module Database where

import Database.PostgreSQL.Simple
import Data.Text (Text(..), unpack)
import Data.ByteString.Char8 (pack)
import Control.Monad (forM_)
import Card
import System.Environment (lookupEnv)

friendsNames :: Connection -> IO [(Only Text)]
friendsNames conn = do
  xs <- query_ conn "select name from friends"
  return xs

friendsAges :: Connection -> IO [(Text, Int)]
friendsAges conn = do
  xs <- query_ conn "select name,age from friends"
  return xs

temp = do
  (Just db_url) <- lookupEnv "DATABASE_URL"
  conn <- connectPostgreSQL (pack db_url)

  execute_ conn "CREATE TABLE IF NOT EXISTS card (title varchar(80), rules text);"
  execute_ conn "INSERT INTO card VALUES ('Xuukuu', 'Roam: +2');"
  execute_ conn "INSERT INTO card VALUES ('Nice Blizzard', 'Crunch all seeds. Players gain $1 for each seed lost.');"

  -- putStrLn "\nOnly names:"
  -- names <- (friendsNames conn)
  -- forM_ names $ \(Only name) ->
  --   putStrLn $ unpack name

  -- putStrLn "\nNames and ages:"
  -- ages <- (friendsAges conn)
  -- forM_ ages $ \(name,age) ->
  --   putStrLn $ unpack name ++ " is " ++ show (age :: Int)

getCards :: IO [Card]
getCards = do
  (Just db_url) <- lookupEnv "DATABASE_URL"
  conn <- connectPostgreSQL (pack db_url)
  cards <- query_ conn "SELECT title,rules FROM card;"
  return $ map (\(t,r) -> Card t r) cards


