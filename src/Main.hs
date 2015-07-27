{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Web.Spock.Safe
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (foldM, mapM)
import Data.Text.Internal (Text)
import Data.Text (unpack, pack)
import Data.Text.Lazy (toStrict)
import qualified Data.Text as Text
import Data.Monoid ((<>))
import Database (temp, getCards, addCard)
import Card
import System.Environment (getArgs)
import Lucid

-- export DATABASE_URL="dbname=splicers user=erik"
-- cabal run _ 8080

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "Nr of args: " ++ show (length args)
  let [_, port] = args
  runSpock (read port) $ spockT id $ do
    get root $ html $ toStrict $ renderText $ do h1_ "Splicers"
                                                 h2_ "An open source collectible card game"
    get ("hello" <//> var) hello
    get "db" $ do
      liftIO temp
      (text "performed db stuff")
    get "cards" $ do
      cards <- liftIO getCards
      renderCards cards
    get "add-card" $ do
      html "<form action='submit-card'>Title: <input type='text' name='title'><br>Rules text: <input type='text' name='rules'><br><input type='submit' value='Submit'></form>"
    get "submit-card" $ do
      (Just title) <- param "title"
      (Just rules) <- param "rules"
      liftIO (addCard title rules)
      html $ "Added card '" <> title <> "'<br><a href='/cards'>Card list</a>"
         
hello :: MonadIO m => Text -> ActionT m a
hello name = html ("Hejsan <a href=\"#\">" <> name <> "</a>.")

renderCards :: MonadIO m => [Card] -> ActionT m a
renderCards cards = do
  html $ css <> (Text.unlines (map renderCard cards))

renderCard :: Card -> Text
renderCard card = "<ul>" <> title card <> " - " <> rules card <> "</ul>"

css = "<style> body { \
      \ background-color: gray; \
      \ color: black; \
      \ margin-top: 100px; \
      \ font-family: Karla, monospace; \
      \ line-height: 18px; \
      \ word-spacing: 2px; \
      \ } \
      \ div { \
      \ margin-left: auto; \
      \ margin-right: auto; \
      \ width: 500px; \
      \ padding: 50px; \
      \ background-color: white; \
      \ -moz-box-shadow: 10px 10px 0 rgba(0, 0, 0, 0.5); \
      \ -webkit-box-shadow: 10px 10px 0 rgba(0, 0, 0, 0.5); \
      \ box-shadow: 10px 10px 0 rgba(0, 0, 0, 0.5); \
      \ margin-bottom: 50px; \
      \ } \
      \ .title { \
      \ background-color: pink; \
      \ font-weight: bolder; \
      \ font-size: 30px; \
      \ } \
      \ .headline { \
      \ font-weight: bold; \
      \ font-size: 16px; \
      \ background-color: pink; \
      \ } \
      \ .text { \
      \ text-indent: 10px; \
      \ font-size: 14px; \
      \ } \
      \ li { \
      \ font-style: italic; \
      \ } \
      \ #background { \
      \ width: 100%; \
      \ height: 100%; \
      \ position: fixed; \
      \ top: 0; \
      \ bottom: 0; \
      \ left: 0; \
      \ right: 0; \
      \ z-index: -1; \
      \ } </style>"      
