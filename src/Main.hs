{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Web.Spock.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Environment (getArgs)
import System.Environment (lookupEnv)
import Data.Text (unpack, pack)
import Data.Text.Lazy (toStrict)
import Database (migrate, getCards, addCard, addFakeData)
import Card
import Rendering
import Lucid

main :: IO ()
main = do
  migrate
  port <- getPort
  runSpock (read port) $ spockT id $ do
    get root $              frontPageRoute
    get "cards" $           cardsRoute
    get "add-card" $        addCardRoute
    get "submit-card" $     submitCardRoute
    get "add-fake-data" $   addFakeDataRoute
    get ("files" <//> var)  getFile

frontPageRoute :: ActionT IO a
frontPageRoute = do
  lucidToSpock renderFrontPage

cardsRoute :: ActionT IO a
cardsRoute = do
  cards <- liftIO getCards
  lucidToSpock $ renderCards cards

addCardRoute =
  lucidToSpock $ renderAddCard

paramOrDefault name defaultValue = do
  maybeValue <- param name
  case maybeValue of
    Just value -> return value
    Nothing -> return defaultValue

submitCardRoute :: ActionT IO a
submitCardRoute = do
  title <- paramOrDefault "title" "untitled"
  rules <- paramOrDefault "rules" ""
  domination <- paramOrDefault "domination" "0"
  cost <- paramOrDefault "cost" "0"
  cardType <- paramOrDefault "cardType" ""
  subType <- paramOrDefault "subType" ""
  gene1 <- paramOrDefault "gene1" ""
  gene2 <- paramOrDefault "gene2" ""
  startMatter <- paramOrDefault "startMatter" "0"
  startCards <- paramOrDefault "startCards" "0"
  flavor <- paramOrDefault "flavor" ""
  designer <- paramOrDefault "designer" "unknown"
  illustration <- paramOrDefault "illustration" ""
  let card = mkCard title
                    rules
                    (read domination)
                    (read cost)
                    cardType
                    subType
                    gene1
                    gene2
                    (read startMatter)
                    (read startCards)
                    flavor
                    designer
                    illustration
  liftIO (addCard card)
  lucidToSpock (renderSubmittedCard title)

addFakeDataRoute = do
  liftIO addFakeData
  lucidToSpock renderAddFakeData

getFile :: MonadIO m => String -> ActionT m a
getFile name = file (pack name) ("./files/" ++ name)

lucidToSpock :: MonadIO m => Html () -> ActionT m a
lucidToSpock t = html $ toStrict $ renderText t

defaultPort :: String
defaultPort = "8080"

getPort :: IO String
getPort = do
  maybePort <- lookupEnv "PORT"
  port <- case maybePort of
    (Just port) -> return port
    Nothing -> do
      putStrLn $ "No PORT string found in environment, using default (" ++ defaultPort ++ ")."
      return defaultPort
  return port
