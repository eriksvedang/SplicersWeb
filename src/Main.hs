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
import Database (migrate, getCards, addCard, addFakeData)
import Card
import System.Environment (getArgs)
import System.Environment (lookupEnv)
import Lucid

main :: IO ()
main = do
  migrate
  (Just port) <- lookupEnv "PORT"
  runSpock (read port) $ spockT id $ do
    get root $              renderFrontPage
    get ("files" <//> var)  getFile
    get "cards" $           cardsRoute
    get "add-card" $        renderAddCard
    get "submit-card" $     submitCardRoute
    get "add-fake-data" $   addFakeDataRoute

getFile :: MonadIO m => String -> ActionT m a
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

renderFrontPage :: ActionT IO a
renderFrontPage = lucidToSpock $ renderPage $ do h1_ "Splicers"
                                                 h2_ "An open source collectible card game"
                                                 p_ "Yeah, it's pretty cool."
                                                 a_ [href_ "/cards"] "Cards"

cardsRoute :: ActionT IO a
cardsRoute = do cards <- liftIO getCards
                lucidToSpock $ renderCards cards

renderCards :: [Card] -> Html ()
renderCards cards = renderPage $ mapM_ renderCard cards

svg path = embed_ [src_ path, type_ "image/svg+xml"]

renderCard :: Card -> Html ()
renderCard card = 
  do div_ [class_ "card ting"] $ do
       div_ [class_ "title"] $ toHtml (title card)
       div_ [class_ "cost"] $ do
         svg "/files/cost.svg"
         span_ $ toHtml (show (cost card))
       div_ [class_ "illustration"] ""
       div_ [class_ "types"] $ do
         div_ [class_ "dominance"] $ do
           span_ $ toHtml (show (dominance card))
         span_ "ting - animal"
       div_ [class_ "ability"] $ do
         with (svg "/files/gen_artificial.svg") [class_ "gene1"]
         with (svg "/files/gen_plant.svg") [class_ "gene1"]
         span_ $ toHtml (rules card)

input name = input_ [type_ "text", name_ name]

renderAddCard :: ActionT IO a
renderAddCard =
  lucidToSpock $ renderPage $ do
    form_ [action_ "submit-card"] $ do
      div_ $ do span_ "Title:"
                input "title"
      div_ $ do span_ "Rules text:"
                input "rules"
      br_ []
      input_ [type_ "submit", value_ "Submit"]

submitCardRoute :: ActionT IO a
submitCardRoute = do
  (Just title) <- param "title"
  (Just rules) <- param "rules"
  liftIO (addCard title rules)
  lucidToSpock (renderSubmitCard title rules)

renderSubmitCard :: Text -> Text -> Html ()
renderSubmitCard title rules = renderPage $ do p_ $ toHtml $ "Title: " <> title
                                               p_ $ toHtml $ "Rules: " <> rules
                                               a_ [href_ "/cards"] "Cards"

addFakeDataRoute = do
  liftIO addFakeData
  lucidToSpock $ renderPage $ p_ "Added fake data!"

