{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Rendering where

import Control.Monad (foldM, mapM_)
import Data.Text.Internal (Text)
import Data.Text (unpack, pack)
import Data.Monoid ((<>))
import Card
import Lucid

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

renderCards :: [Card] -> Html ()
renderCards cards = renderPage $ mapM_ renderCard cards

svg path = embed_ [src_ path, type_ "image/svg+xml"]

renderCard :: Card -> Html ()
renderCard card =
  case (cardType card) of
  Ting -> renderTing card
  Event -> renderEvent card
  Biom -> renderBiom card
  Mutation -> renderMutation card
  Splicer -> renderSplicer card

illustrationDiv = div_ [class_ "illustration"] ""

typesDiv :: Text -> Text -> Int -> Html ()
typesDiv cardType subType dominance = do
  div_ [class_ "types"] $ do
    div_ [class_ "dominance"] $ do
      span_ $ toHtml (show dominance)
    span_ $ toHtml $ cardType <> subPart
      where subPart = case subType of
              "" -> ""
              _ -> " - " <> subType

renderTing :: Card -> Html ()
renderTing card = 
  do div_ [class_ "card ting"] $ do
       div_ [class_ "title"] $ toHtml (title card)
       div_ [class_ "cost"] $ do
         svg "/files/cost.svg"
         span_ $ toHtml (show (cost card))
       illustrationDiv
       typesDiv "ting" (subType card) (dominance card)
       div_ [class_ "ability"] $ do
         with (svg "/files/gen_artificial.svg") [class_ "gene1"]
         with (svg "/files/gen_plant.svg") [class_ "gene1"]
         span_ $ toHtml (rules card)

renderEvent :: Card -> Html ()
renderEvent card = 
  do div_ [class_ "card event"] $ do
       div_ [class_ "title"] $ toHtml (title card)
       div_ [class_ "cost"] $ do
         svg "/files/cost.svg"
         span_ $ toHtml (show (cost card))
       illustrationDiv
       typesDiv "event" (subType card) 0
       div_ [class_ "ability"] $ do
         span_ $ toHtml (rules card)

renderBiom :: Card -> Html ()
renderBiom card = 
  do div_ [class_ "card biom"] $ do
       div_ [class_ "title"] $ toHtml (title card)
       div_ [class_ "windom"] $ toHtml (show (dominance card))
       div_ [class_ "cost"] $ do
         svg "/files/cost.svg"
         span_ $ toHtml (show (cost card))
       illustrationDiv
       typesDiv "biom" (subType card) 0
       div_ [class_ "ability"] $ do
         span_ $ toHtml (rules card)

renderMutation :: Card -> Html ()
renderMutation card = 
  do div_ [class_ "card mutation"] $ do
       div_ [class_ "title"] $ toHtml (title card)
       illustrationDiv
       typesDiv "mutation" (subType card) 0
       div_ [class_ "ability"] $ do
         span_ $ toHtml (rules card)

renderSplicer :: Card -> Html ()
renderSplicer card = 
  do div_ [class_ "card splicer"] $ do
       div_ [class_ "title"] $ toHtml (title card)
       div_ [class_ "cost"] $ do
         svg "/files/cost.svg"
         span_ $ toHtml (show (cost card))
       illustrationDiv
       typesDiv "splicer" (subType card) 0
       div_ [class_ "matter"] $ do
         svg "/files/cost.svg"
         span_ "4"
       div_ [class_ "carddraw"] $ do
         svg "/files/carddraw.svg"
         span_ "4"
       div_ [class_ "ability"] $ do
         span_ $ toHtml (rules card)

input name = input_ [type_ "text", name_ name]

renderAddCard :: Html ()
renderAddCard =
  renderPage $ do
    form_ [action_ "submit-card"] $ do
      div_ $ do span_ "Title:"
                input "title"
      div_ $ do span_ "Rules text:"
                input "rules"
      br_ []
      input_ [type_ "submit", value_ "Submit"]

renderSubmitCard :: Text -> Text -> Html ()
renderSubmitCard title rules = renderPage $ do p_ $ toHtml $ "Title: " <> title
                                               p_ $ toHtml $ "Rules: " <> rules
                                               a_ [href_ "/cards"] "Cards"

renderAddFakeData :: Html ()
renderAddFakeData = do
  renderPage $ p_ "Added fake data!"

