{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Spock.Safe
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (foldM, mapM)
import Data.Text.Internal (Text)
import Data.Text (unpack, pack)
import Data.Monoid ((<>))
import Database (temp, getCards)
import Card
import System.Environment (getArgs)

main :: IO ()
main = do
  [port] <- getArgs
  runSpock (read port) $ spockT id $ do
    get root $ html "<h1>Splicers</h1>"
    get ("hello" <//> var) hello
    get "db" $ do
      liftIO temp
      (text "performed db stuff")
    get "cards" $ do
      cards <- liftIO getCards
      renderCards cards
         
hello :: MonadIO m => Text -> ActionT m a
hello name = html ("Hejsan <a href=\"#\">" <> name <> "</a>.")

renderCards :: MonadIO m => [Card] -> ActionT m a
renderCards cards = do
  html $ pack (show (map renderCard cards))

renderCard :: Card -> Text
renderCard card = "<h1>" <> title card <> "</h1>"


