{-# LANGUAGE OverloadedStrings #-}

module Database where

import Database.PostgreSQL.Simple
import Data.Text (Text(..), unpack)
import Control.Monad (forM_)
import Card

friendsNames :: Connection -> IO [(Only Text)]
friendsNames conn = do
  xs <- query_ conn "select name from friends"
  return xs

friendsAges :: Connection -> IO [(Text, Int)]
friendsAges conn = do
  xs <- query_ conn "select name,age from friends"
  return xs

temp = do
  conn <- connectPostgreSQL "dbname=culture user=erik"

  execute_ conn "CREATE TABLE IF NOT EXISTS friends (name varchar(80), age int);"
  execute_ conn "INSERT INTO friends VALUES ('Erik', 27);"
  execute_ conn "INSERT INTO friends VALUES ('Niklas', 32);"

  putStrLn "\nOnly names:"
  names <- (friendsNames conn)
  forM_ names $ \(Only name) ->
    putStrLn $ unpack name

  putStrLn "\nNames and ages:"
  ages <- (friendsAges conn)
  forM_ ages $ \(name,age) ->
    putStrLn $ unpack name ++ " is " ++ show (age :: Int)

getCards :: IO [Card]
getCards = do
  conn <- connectPostgreSQL "dbname=splicers user=erik"
  cards <- query_ conn "SELECT title,rules FROM card;"
  return $ map (\(t,r) -> Card t r) cards


