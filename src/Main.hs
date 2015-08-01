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
    get root $ lucidToSpock renderFrontPage
    get ("files" <//> var) getFile
    get "cards" $ do cards <- liftIO getCards
                     lucidToSpock $ renderCards cards
    get "add-card" $ lucidToSpock renderAddCard
    get "submit-card" $ do
      (Just title) <- param "title"
      (Just rules) <- param "rules"
      liftIO (addCard title rules)
      lucidToSpock (renderSubmitCard title rules)

getFile name = file (pack name) ("./files/" ++ name)

lucidToSpock :: MonadIO m => Html () -> ActionT m a
lucidToSpock t = html $ toStrict $ renderText t

css :: Text -> Html ()
css name = link_ [rel_ "stylesheet", type_ "text/css", href_ name]

allCSS :: Html ()
allCSS = do (css "/files/styles.css")
            (css "/files/card.css")
            (css "http://fonts.googleapis.com/css?family=Karla:400,700,400italic,700italic")

renderPage :: Html () -> Html ()
renderPage body = do head_ $ allCSS
                     body_ $ body

renderFrontPage :: Html ()
renderFrontPage = renderPage $ do h1_ "Splicers"
                                  h2_ "An open source collectible card game"
                                  p_ "Yeah, it's pretty cool."
                                  a_ [href_ "/cards"] "Cards"

renderCards cards = mapM_ renderCard cards

renderCard :: Card -> Html ()
renderCard card = do div_ $ do p_ $ toHtml (title card)
                               p_ $ toHtml (rules card)

renderAddCard =
  renderPage $ do
    form_ [action_ "submit-card"] $ do
      div_ "Title:"
      input_ [type_ "text", name_ "title"]
      div_ "Rules text:"
      input_ [type_ "text", name_ "rules"]
      br_ []
      input_ [type_ "submit", value_ "Submit"]

renderSubmitCard :: Text -> Text -> Html ()
renderSubmitCard title rules = renderPage $ do p_ $ toHtml $ "Title: " <> title
                                               p_ $ toHtml $ "Rules: " <> rules

