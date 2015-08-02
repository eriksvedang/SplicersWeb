{-# LANGUAGE OverloadedStrings #-}

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
  (Just port) <- lookupEnv "PORT"
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

submitCardRoute :: ActionT IO a
submitCardRoute = do
  (Just title) <- param "title"
  (Just rules) <- param "rules"
  liftIO (addCard title rules)
  lucidToSpock (renderSubmitCard title rules)

addFakeDataRoute = do
  liftIO addFakeData
  lucidToSpock renderAddFakeData

getFile :: MonadIO m => String -> ActionT m a
getFile name = file (pack name) ("./files/" ++ name)

lucidToSpock :: MonadIO m => Html () -> ActionT m a
lucidToSpock t = html $ toStrict $ renderText t
