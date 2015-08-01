{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Web.Spock.Safe
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (foldM, mapM_)
import Data.Text.Internal (Text)
import Data.Text (unpack, pack)
import Data.Text.Lazy (toStrict)
import qualified Data.Text as Text
import Data.Monoid ((<>))
import Database (temp, getCards, addCard)
import Card
import System.Environment (getArgs)
import System.Environment (lookupEnv)
import Lucid

main :: IO ()
main = do
  (Just port) <- lookupEnv "PORT"
  runSpock (read port) $ spockT id $ do
    get root $ html $ toStrict $ renderText $ renderFrontPage
    get ("hello" <//> var) hello
    get ("files" <//> var) (\name -> file "A file..?" ("./files/" ++ name))
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

render t = html $ toStrict $ renderText t

renderFrontPage = do head_ $ do link_ [rel_ "stylesheet", type_ "text/css", href_ "/files/styles.css"]
                                link_ [rel_ "stylesheet", type_ "text/css", href_ "/files/card.css"]
                                link_ [href_ "http://fonts.googleapis.com/css?family=Karla:400,700,400italic,700italic",
                                       rel_ "stylesheet",
                                       type_ "text/css"]
                                body_ $ do h1_ "Splicers"
                                           h2_ "An open source collectible card game"
                                           p_ "Yeah, it's pretty cool."
                                           ---a_ [href_ "/cards"] "Cards

hello :: MonadIO m => String -> ActionT m a
hello name = render $ do (h1_ $ "Hello!") :: Html ()

renderCards :: MonadIO m => [Card] -> ActionT m a
renderCards cards = do
  html $ Text.unlines (map renderCard cards)

renderCard :: Card -> Text
renderCard card = "<ul>" <> title card <> " - " <> rules card <> "</ul>"
