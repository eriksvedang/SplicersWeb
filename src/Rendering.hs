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
renderFrontPage = renderPage $ do div_ [id_ "logo"] $ return ()
                                  h2_ "An open source collectible card game"
                                  ul_ $ do
                                    li_ $ a_ [href_ "/cards"] "Cards"
                                    li_ $ a_ [href_ "/add-card"] "Add a card"
                                    li_ $ a_ [href_ "/user"] "User page"

renderCards :: [Card] -> Html ()
renderCards cards = renderPage $ mapM_ (renderCard AsLink) cards

data RenderCardMode = AsLink | NoLink

renderSingleCardPage :: Text -> [Card] -> Html ()
renderSingleCardPage title cards =
  renderPage $ do
    h1_ [] (toHtml title)
    h2_ [] (toHtml $ "Designed by " <> (designer (head cards)))
    div_ [] $ do
      mapM_ (\card -> p_ [] (renderCard NoLink card)) cards
    a_ [href_ "/cards"] "Cards"

svg :: Text -> Html ()
svg path = embed_ [src_ path, type_ "image/svg+xml"]

renderCard :: RenderCardMode -> Card -> Html ()
renderCard cardMode card =
  case cardMode of
  AsLink -> do
    a_ [href_ $ "card/" <> (title card)] renderedCard
  NoLink -> do
    p_ [] renderedCard
  where
    renderedCard = 
      case (cardType card) of
      Ting -> renderTing card
      Event -> renderEvent card
      Biom -> renderBiom card
      Mutation -> renderMutation card
      Splicer -> renderSplicer card

illustrationDiv :: Card -> Html ()
illustrationDiv card =
  div_ [class_ "illustration"] $ do
    img_ [src_ (illustration card)]

typesDiv :: Text -> Text -> Html ()
typesDiv cardType subType = do
  div_ [class_ "types"] $ do
    span_ $ toHtml $ cardType <> subPart
      where subPart = case subType of
              "" -> ""
              _ -> " - " <> subType
              
typesDivWithDomination :: Text -> Text -> Int -> Html ()
typesDivWithDomination cardType subType dominance = do
  div_ [class_ "types"] $ do
    div_ [class_ "dominance"] $ do
      span_ $ toHtml (show dominance)
    span_ $ toHtml $ cardType <> subPart
      where subPart = case subType of
              "" -> ""
              _ -> " - " <> subType

flavorText :: Card -> Html ()
flavorText card = p_ [class_ "flavor"] $ toHtml (flavor card)

renderTing :: Card -> Html ()
renderTing card = 
  do div_ [class_ "card ting"] $ do
       div_ [class_ "title"] $ toHtml (title card)
       illustrationDiv card
       typesDivWithDomination "ting" (subType card) (dominance card)
       div_ [class_ "ability"] $ do
         img_ [src_ "/files/gen_artificial.png", class_ "gene1"]
         img_ [src_ "/files/gen_artificial.png", class_ "gene1"]
         span_ $ toHtml (rules card)
         flavorText card 

renderEvent :: Card -> Html ()
renderEvent card = 
  do div_ [class_ "card event"] $ do
       div_ [class_ "title"] $ toHtml (title card)
       illustrationDiv card
       typesDiv "event" (subType card)
       div_ [class_ "ability"] $ do
         span_ $ toHtml (rules card)
         flavorText card 

renderBiom :: Card -> Html ()
renderBiom card = 
  do div_ [class_ "card biom"] $ do
       div_ [class_ "title"] $ toHtml (title card)
       illustrationDiv card
       typesDivWithDomination "biom" (subType card) (dominance card)
       div_ [class_ "ability"] $ do
         span_ $ toHtml (rules card)
         --flavorText card 

renderMutation :: Card -> Html ()
renderMutation card = 
  do div_ [class_ "card mutation"] $ do
       illustrationDiv card
       typesDiv "mutation" (subType card)
       div_ [class_ "title"] $ toHtml (title card)
       div_ [class_ "ability"] $ do
         span_ $ toHtml (rules card)
         flavorText card 

renderSplicer :: Card -> Html ()
renderSplicer card = 
  do div_ [class_ "card splicer"] $ do
       div_ [class_ "carddraw"] $ do
         svg "/files/carddraw.svg"
         span_ $ toHtml (show (startCards card))
       div_ [class_ "title"] $ toHtml (title card)
       illustrationDiv card
       typesDiv "splicer" (subType card)
       div_ [class_ "ability"] $ do
         span_ $ toHtml (rules card)
         flavorText card

field :: Text -> Text -> Text -> Text -> Html ()
field name helpText inputType defaultValue =
  div_ $ do span_ (toHtml name)
            input_ [ type_ inputType
                   , name_ name
                   , value_ defaultValue
                   ]
            span_ (toHtml helpText)

renderAddCard :: Text -> Html ()
renderAddCard username =
  renderPage $ do
    form_ [action_ "submit-card"] $ do
      field "title" "" "text" ""
      field "rules" "" "text" ""
      field "domination" "" "number" "0"
      field "cost" "" "number" "0"
      field "cardType" " (Ting, Biom, Event, Mutation, Splicer)" "text" "Ting"
      field "subType" " (i.e. Animal, Plant...)" "text" ""
      field "gene1" "" "text" ""
      field "gene2" "" "text" ""
      field "startMatter" "" "number" "4"
      field "startCards" "" "number" "4"
      field "flavor" "" "text" ""
      span_ "designer"
      input_ [type_ "text", name_ "designer", value_ username, readonly_ ""]
      field "illustration" " (URL)" "text" ""
      br_ []
      input_ [type_ "submit", value_ "Submit"]

renderSubmittedCard :: Text -> Html ()
renderSubmittedCard title =
  renderPage $ do
    p_ $ toHtml $ "The card " <> title <> " was added!"
    a_ [href_ "/cards"] "Cards"

renderAddFakeData :: Html ()
renderAddFakeData = do
  renderPage $ p_ "Added fake data!"

renderUserPage :: Text -> Html ()
renderUserPage username = do
  renderPage $ do
    h1_ (toHtml username)
    a_ [href_ "/"] "Front page"
    a_ [href_ "/logout"] "Log out"

renderLoginFormFull :: Html ()
renderLoginFormFull = do
  renderPage $ do
    renderLoginForm ""

renderLoginForm :: Text -> Html ()
renderLoginForm nextPage = do
  form_ [action_ "submit-login"] $ do
    span_ "Username "
    input_ [type_ "text", name_ "username"]
    br_ []
    span_ "Password "
    input_ [type_ "password", name_ "password"]
    input_ [type_ "hidden", name_ "next", value_ nextPage]
    input_ [type_ "submit", value_ "Login"]

renderMustLogIn :: Text -> Text -> Html ()
renderMustLogIn helpText nextPage = do
  renderPage $ do
    p_ (toHtml helpText)
    renderLoginForm nextPage

renderSucceededToLogin :: Html ()
renderSucceededToLogin = do
  renderPage $ do
    p_ "You are logged in!"

renderFailedToLogin :: Html ()
renderFailedToLogin = do
  renderPage $ do
    p_ "Failed to login, invalid password or username."

renderLogout :: Html ()
renderLogout = do
  renderPage $ do
    p_ "You have been logged out."
