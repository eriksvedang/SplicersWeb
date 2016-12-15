{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Rendering where

import Control.Monad (foldM, mapM_)
import Data.Text.Internal (Text)
import Data.Text (unpack, pack)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Card
import Keyword
import Lucid
import Deck

css :: Text -> Html ()
css name = link_ [rel_ "stylesheet", type_ "text/css", href_ name]

allCSS :: Html ()
allCSS = do (css "/files/styles.css")
            (css "/files/card.css")
            (css "http://fonts.googleapis.com/css?family=Karla:400,700,400italic,700italic")

renderPage :: Maybe Deck -> Html () -> Html ()
renderPage activeDeck body = do head_ $ do
                                  (script_ [src_ "/files/jquery.min.js"] "")
                                  (script_ [src_ "/files/scripts.js"] "")
                                  (script_ [src_ "https://cdn.rawgit.com/showdownjs/showdown/1.5.1/dist/showdown.min.js"] "")
                                  (script_ [src_ "//cdnjs.cloudflare.com/ajax/libs/jquery-cookie/1.4.1/jquery.cookie.min.js"] "")
                                  allCSS
                                body_ $ do
                                  renderSmallMenu activeDeck
                                  body


renderFrontPage :: Maybe Deck -> Html ()
renderFrontPage activeDeck = renderPage activeDeck $
  do div_ [id_ "page"] $ do
       renderMenu
       div_ [] $ do
         article_ (toHtml "Hello")

cornerWidget :: Maybe Deck -> Html ()
cornerWidget deckToEdit = do
  case deckToEdit of
    Just deck -> a_ [class_ "editing randomcolor", href_ (T.append "/deck/" ((pack . show . deckId) deck))]
                 (toHtml (T.append "Editing: " (deckName deck)))
    Nothing -> span_ [class_ "editing"] "No deck to edit"
  div_ [id_ "filter", class_ "randomcolor"] $ do
  span_ [] (toHtml "filter cards →")
  input_ [ type_ "text", name_ "filter"]
  
renderCards :: [Card] -> Maybe Deck -> [Card] -> Html ()
renderCards cards activeDeck cardsInDeck = renderPage activeDeck $
  do a_ [ href_ "add-card"] $ do
       div_ [class_ "add"] $ do
         span_ [] (toHtml "+ Create a card")
     mapM_ (\card -> renderCard (if ((title card) `elem` (map title cardsInDeck)) then InDeckSelection else AsLink) card) cards
                                                           

data RenderCardMode = AsLink | NoLink | InDeckSelection

renderSingleCardPage :: Maybe Deck -> Text -> [Card] -> Html ()
renderSingleCardPage activeDeck title cards =
  renderPage activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
        h1_ [] (toHtml title)
        span_ [] (toHtml $ "Designed by " <> (designer (head cards)))
        a_ [href_ "/cards"] "Cards"
      div_ [class_ "preview randomcolor"] $ do
        mapM_ (\card -> p_ [] (renderCard NoLink card)) cards

svg :: Text -> Html ()
svg path = embed_ [src_ path, type_ "image/svg+xml"]

renderCard :: RenderCardMode -> Card -> Html ()
renderCard cardMode card =
  case cardMode of
    AsLink -> do
      a_ [class_ "cardLink", href_ $ "/card/" <> (title card)] renderedCard
    NoLink -> do
      p_ [] renderedCard
    InDeckSelection -> do
      a_ [class_ "cardLink selected", href_ $ "#" <> (title card)] renderedCard
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
  div_ [class_ "illustration randomcolor"] $ do
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
         div_ [class_ (T.append "gene1 " (pack (show (gene1 card))))] $ return ()
         div_ [class_ (T.append "gene2 " (pack (show (gene2 card))))] $ return ()
         span_ $ toHtml (rules card)
         flavorText card

renderEvent :: Card -> Html ()
renderEvent card =
  do div_ [class_ "card event"] $ do
       div_ [class_ "title"] $ toHtml (title card)
       illustrationDiv card
       typesDiv "event" (subType card)
       div_ [class_ "ability"] $ do
         div_ [class_ (T.append "gene1 " (pack (show (gene1 card))))] $ return ()
         div_ [class_ (T.append "gene2 " (pack (show (gene2 card))))] $ return ()
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
         div_ [class_ (T.append "gene1 " (pack (show (gene1 card))))] $ return ()
         div_ [class_ (T.append "gene2 " (pack (show (gene2 card))))] $ return ()
         span_ $ toHtml (rules card)
         flavorText card

renderSplicer :: Card -> Html ()
renderSplicer card =
  do div_ [class_ "card splicer"] $ do
       div_ [class_ "carddraw"] $ do
         span_ $ toHtml (show (startCards card))
       div_ [class_ "title"] $ toHtml (title card)
       illustrationDiv card
       typesDiv "splicer" (subType card)
       div_ [class_ "ability"] $ do
         span_ $ toHtml (rules card)
         flavorText card

field :: Text -> Text -> Text -> Text -> Text -> Html ()
field name heading helpText inputType defaultValue =
  div_ $ do span_ (toHtml heading)
            i_ (toHtml helpText)
            br_ []
            input_ [ type_ inputType
                   , name_ name
                   , value_ defaultValue
                   ]

            br_ []


renderAddCard :: Maybe Deck -> Text -> Html ()
renderAddCard activeDeck username =
  renderPage activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
        h1_ (toHtml "Create a card")
        form_ [action_ "submit-card"] $ do
          span_ (toHtml "Select a card type: ")
          select_ [name_ "cardType"] $ do
            option_ [value_ "Ting"] (toHtml "Ting")
            option_ [value_ "Biom"] (toHtml "Biom")
            option_ [value_ "Event"] (toHtml "Event")
            option_ [value_ "Mutation"] (toHtml "Mutation")
            option_ [value_ "Splicer"] (toHtml "Splicer")

          br_ []
          br_ []
          field "title" "Title" "" "text" ""
          field "subType" "Subtype" " (i.e. Animal, Plant...)" "text" ""
          field "domination" "Dominance" " (0-10)" "number" "0"
          field "cost" "cost" "" "number" "0"
          div_ [id_ "genes"] $ do
            span_ (toHtml "Select genes: ")
            select_ [name_ "gene1"] $ do
              option_ [value_ "NoGene"] (toHtml "Empty")
              option_ [value_ "Air"] (toHtml "Air")
              option_ [value_ "Artificial"] (toHtml "Artificial")
              option_ [value_ "Bug"] (toHtml "Bug")
              option_ [value_ "Fungi"] (toHtml "Fungi")
              option_ [value_ "Mini"] (toHtml "Mini")
              option_ [value_ "Plant"] (toHtml "Plant")
              option_ [value_ "Nautic"] (toHtml "Nautic")
              option_ [value_ "Sinister"] (toHtml "Sinister")
              option_ [value_ "Land"] (toHtml "Land")
            select_ [name_ "gene2"] $ do
              option_ [value_ "NoGene"] (toHtml "Empty")
              option_ [value_ "Air"] (toHtml "Air")
              option_ [value_ "Artificial"] (toHtml "Artificial")
              option_ [value_ "Bug"] (toHtml "Bug")
              option_ [value_ "Fungi"] (toHtml "Fungi")
              option_ [value_ "Mini"] (toHtml "Mini")
              option_ [value_ "Plant"] (toHtml "Plant")
              option_ [value_ "Nautic"] (toHtml "Nautic")
              option_ [value_ "Sinister"] (toHtml "Sinister")
              option_ [value_ "Land"] (toHtml "Land")
          br_ []
          field "startMatter" """" "number" "4"
          field "startCards" "Cards" " (The number of cards you start with)" "number" "4"
          field "rules" "Rule text" " (example: @: Roam)" "text" ""
          field "flavor" "Flavour text" "" "text" ""
          input_ [type_ "text", name_ "designer", value_ username, readonly_ ""]
          field "illustration" "Image" " (URL)" "text" ""
          br_ []
          input_ [class_ "button", type_ "submit", value_ "Submit"]
      div_ [class_ "preview randomcolor"] $ do
        renderCard NoLink (Card "title" "rules" 0 Ting "type" NoGene NoGene 4 4 "" username "")


renderSubmittedCard :: Maybe Deck -> Text -> Html ()
renderSubmittedCard activeDeck title =
  renderPage activeDeck $ do
    p_ $ toHtml $ "The card " <> title <> " was added!"
    a_ [href_ "/cards"] "Cards"

renderAddFakeData :: Maybe Deck -> Html ()
renderAddFakeData activeDeck = do
  renderPage activeDeck $ p_ "Added fake data!"

renderPlayerPage :: Maybe Deck -> Text -> [Text] -> [Deck] -> Html ()
renderPlayerPage activeDeck username myCardTitles myDecks = do
  renderPage activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
        h1_ [class_ "randomcolor"] (toHtml username)
        h3_ "Cards by me"
        mapM_ (\cardTitle -> li_ $ a_ [href_ $ pack ("/card/" ++ unpack cardTitle)] (toHtml cardTitle)) myCardTitles
        h3_ "Decks by me"
        mapM_ (\(deck) -> li_ $ do a_ [href_ $ pack ("/deck/" ++ show (deckId deck))] (toHtml $ deckName deck)
                                   span_ [] (toHtml " ")
                                   a_ [href_ $ T.append "/edit-deck/" ((pack . show . deckId) deck)] (toHtml "Edit")
                                   a_ [href_ $ T.append "/delete-deck?deckId=" ((pack . show . deckId) deck)] (toHtml "Delete"))
          myDecks
        a_ [href_ "/new-deck", class_ "button"] "Create a new deck"
        a_ [href_ "/", class_ "button"] "Front page"
        a_ [href_ "/logout", class_ "button"] "Log out"

renderDeckPage :: Maybe Deck -> Deck -> [Card] -> Html ()
renderDeckPage activeDeck deck cards = do
  renderPage activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content deckedit"] $ do
        input_ [id_ "deckname", class_ "randomcolor", value_ (deckName deck), name_ "deckname"]
        input_ [name_ "deckid", style_ "float:right; background: black; color: white;", value_ ((pack . show . deckId) deck), readonly_ ""]
        span_ [class_ "whileediting", style_ "display:none;"] (toHtml "Currently editing. Click a card to remove it from your deck. Click the title to change it. ")
        a_ [class_ "whileediting button", style_ "display:none;",  href_ "#", onclick_ "removeCookie()"] "Finish editing"
        a_ [class_ "whileediting button", style_ "display:none;",  href_ "#", onclick_ "deleteDeck()"] "Delete Deck"
        a_ [href_ (T.append "/edit-deck/" ((pack . show . deckId) deck))] $ do
                                                                     div_ [class_ "add"] $ do
                                                                       span_ [] (toHtml "Add more cards to deck")
        mapM_ (renderCard InDeckSelection) cards

renderNoSuchDeckPage :: Maybe Deck -> Html ()
renderNoSuchDeckPage activeDeck = do
  renderPage activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
        p_ (toHtml "No deck with that id was found.")

renderSignupForm :: Maybe Deck -> Html ()
renderSignupForm activeDeck = do
  renderPage activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
        form_ [action_ "submit-signup"] $ do
          h1_ "Signup"
          span_ "Username "
          input_ [type_ "text", name_ "username"]
          br_ []
          span_ "Email "
          input_ [type_ "text", name_ "email"]
          br_ []
          span_ "Password "
          input_ [type_ "password", name_ "password"]
          input_ [type_ "hidden", name_ "next", value_ "user"]
          br_ []
          input_ [class_ "button", type_ "submit", value_ "Sign up!"]
          
renderFailSignup :: Maybe Deck -> Html ()
renderFailSignup activeDeck = do
  renderPage activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
        h1_ "Failed to sign up"
        p_ "Perhaps that username is taken."

renderLoginFormFull :: Maybe Deck -> Html ()
renderLoginFormFull activeDeck = do
  renderPage activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
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
    br_ []
    input_ [class_ "button", type_ "submit", value_ "Login"]

renderMustLogIn :: Maybe Deck -> Text -> Text -> Html ()
renderMustLogIn activeDeck helpText nextPage = do
  renderPage activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
        p_ (toHtml helpText)
        renderLoginForm nextPage
        p_ [] "Don't have a player account?"
        a_ [href_ "/signup", class_ "button"] "Sign up here!"

renderSucceededToLogin :: Maybe Deck -> Html ()
renderSucceededToLogin activeDeck = do
  renderPage activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
          p_ "You are logged in!"

renderFailedToLogin :: Maybe Deck -> Html ()
renderFailedToLogin activeDeck = do
  renderPage activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
          p_ "Failed to login, invalid password or username."

renderLogout :: Maybe Deck -> Html ()
renderLogout activeDeck = do
  renderPage activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
          p_ "You have been logged out."

renderKeywordPage :: Maybe Deck -> [Keyword] -> Html ()
renderKeywordPage activeDeck keywords = do
  renderPage activeDeck $ do
    mapM_ renderKeyword keywords

renderKeyword :: Keyword -> Html ()
renderKeyword kw = do h2_ (toHtml (keywordName kw))
                      p_ (toHtml (keywordRules kw))

renderRulesDocument :: Maybe Deck -> Text -> Html ()
renderRulesDocument activeDeck rulesText = do
  renderPage activeDeck $ do
    div_ [id_ "page"] $ do
      renderMenu
      div_ [] $ do
        article_ [class_ "markdown"] (toHtml rulesText)

renderMenu :: Html ()
renderMenu = do
      div_ [id_ "logo", class_ "randomcolor"] $ do
        img_ [src_ "/files/logo.png", id_ "logobg"]
        img_ [src_ "/files/logo_star.png", id_ "star"]
      h2_ [class_ "randomcolor"] "An open source collectible card game"
      div_ [id_ "menu"] $ do
        renderMenuItems


renderSmallMenu :: Maybe Deck -> Html ()
renderSmallMenu activeDeck = do
      div_ [id_ "smallmenu"] $ do
        renderMenuItems
        cornerWidget activeDeck

renderMenuItems :: Html ()
renderMenuItems = do
  a_ [class_ "menu-link randomcolor", href_ "/"] $ toHtml "Start"
  a_ [class_ "menu-link randomcolor", href_ "/rules"] $ toHtml "Rules"
  a_ [class_ "menu-link randomcolor", href_ "/cards"] $ toHtml "Cards"
  a_ [class_ "menu-link randomcolor", href_ "/player"] $ toHtml "Player"

renderError :: Maybe Deck -> Text -> Html ()
renderError activeDeck message = do
  renderPage activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
        p_ (toHtml message)
