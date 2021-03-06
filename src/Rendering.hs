{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Rendering where

import Control.Monad (foldM, mapM_)
import Data.Text.Internal (Text)
import Data.Text (unpack, pack)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Debug.Trace (trace)
import Card
import Keyword
import Lucid
import Deck
import Util

css :: Text -> Html ()
css name = link_ [rel_ "stylesheet", type_ "text/css", href_ name]

allCSS :: Html ()
allCSS = do (css "/files/styles.css")
            (css "/files/card.css")

renderPage :: Text -> Maybe Deck -> Html () -> Html ()
renderPage pageTitle activeDeck body =
  do head_ $ do
       (link_ [rel_ "icon", type_ "image/png", href_ "/files/favicon.png"])
       (title_ (toHtml $ "Splicers - " <> pageTitle))
       (script_ [src_ "/files/jquery.min.js"] "")
       (script_ [src_ "/files/scripts.js"] "")
       (script_ [src_ "https://cdn.rawgit.com/showdownjs/showdown/1.5.1/dist/showdown.min.js"] "")
       (script_ [src_ "//cdnjs.cloudflare.com/ajax/libs/jquery-cookie/1.4.1/jquery.cookie.min.js"] "")
       allCSS
     body_ $ do
       renderSmallMenu activeDeck
       body

renderFrontPage :: Maybe Deck -> Html ()
renderFrontPage activeDeck = renderPage "Welcome" activeDeck $
  do div_ [id_ "page"] $ do
       renderMenu
       div_ [] $ do
         article_ $ do
           h2_ "Welcome!"
           p_ $ do span_ "This is splicers, a game where anyone can make their own cards and play against each other. The website makes it super easy to manage your designs and print them out. What are you waiting for?"
           p_ $ do a_ [href_ "/signup"] "Sign up now"
                   span_ " and start creating!"
           br_ []
           img_ [src_ "/files/splash.jpg", class_ "splash"]
           
           h2_ "Motivation"
           p_ "Collectible card games (like Magic the Gathering or Pokémon) are truly awesome but they also have some problems. First of all they cost a lot of money to play since the cards have to be bought in small booster packs or from people who have made it their business to trade these cards. Secondly these games are made by a small group of people who curates the pool of cards. This is obviously great for the quality of the game - they work with this all day long after all - but it also means that a lot of creativity is wasted. YOUR creativity that is."
           p_ "This is why we made Splicers! It is a collectible card game where everyone is allowed to contribute their ideas for cards. Anything goes, although there are also guidelines to keep the game somewhat cohesive and understandable. If you don't want to come up with cards, that's cool too - you can just create decks with the existing cards and play against others!"
           h2_ "Where to go next?"
           p_ $ do
             span_ "You should definitely check out the "
             a_ [href_ "/cards"] "cards"
             span_ " that people have made. Hopefully that makes you wanna read the "
             a_ [href_ "/rules"] "rules"
             span_ " so you can play the game!"
           p_ $ do
             span_ "You can also check out our "
             a_ [href_ "https://github.com/eriksvedang/SplicersWeb"] (toHtml " GitHub repository")
             span_ " with all the code for this website, or join the "
             a_ [href_ "https://discord.gg/jjmZCEJ"] (toHtml "Discord channel")
             span_ " where we discuss the development of the game."

           h2_ "Made by"
           p_ "The Splicers project (including the rules and this website) was started in the summer of 2015 by Ossian Borén and Erik Svedäng, two game designers from Gothenburg, Sweden."

cornerWidget :: Maybe Deck -> Html ()
cornerWidget deckToEdit = do
  div_ [id_ "filter", class_ "randomcolor", style_ "display: none;"] $ do
    select_ [ type_ "text", name_ "filterType"] $ do
      option_ [ value_ "title", selected_ "", hidden_ ""] (toHtml "Filter by...")
      option_ [ value_ "title" ] (toHtml "Title")
      option_ [ value_ "cardType" ] (toHtml "Card Type")
      option_ [ value_ "types" ] (toHtml "Subtype")
      option_ [ value_ "genes" ] (toHtml "Genes")
      option_ [ value_ "ability" ] (toHtml "Rules")
      option_ [ value_ "designer" ] (toHtml "Designer")
    span_ [] (toHtml "→")
    input_ [ type_ "text", name_ "filter"]
  case deckToEdit of
    Just deck -> a_ [class_ "editing randomcolor", href_ ("/deck/" <> (showAsText . deckId) deck)]
                 (toHtml ("Editing: " <> (deckName deck)))
    Nothing -> a_ [class_ "editing", href_ "/player"] "No deck to edit"

renderCards :: [Card] -> Maybe Deck -> [Card] -> Html ()
renderCards cards activeDeck cardsInDeck = renderPage "Cards" activeDeck $ do
  div_ [ class_ "deckstatus randomcolor"] $ do
    span_ [class_ "whileediting deckcounter", style_ "display:none;"] (toHtml "blurgh")
    br_ [class_ "whileediting", style_ "display:none;"]
    span_ [class_ "whileediting", style_ "display:none;"] (toHtml "Click a card to add or remove it from your deck.")

  a_ [ href_ "add-card"] $ do
       div_ [class_ "add"] $ do
         span_ [] (toHtml "Create a card")
  mapM_ (\card -> renderCard (if ((title card) `elem` (map title cardsInDeck)) then InDeckSelection else AsLink) card) cards

data RenderCardMode = AsLink | NoLink | InDeckSelection | UnderConstruction

renderSingleCardPage :: Maybe Deck -> Text -> [Card] -> Html ()
renderSingleCardPage activeDeck title cards =
  renderPage title activeDeck $ do
    div_ [class_ "window"] $ do
      let card = head cards
      div_ [class_ "content"] $ do
        h1_ [] (toHtml title)
        span_ [] (toHtml $ "✏️  " <> (designer (head cards)))
        br_ []
        a_ [href_ ("/add-card/?title=" <> title
                   <> "&rules=" <> (rules card)
                   <> "&domination=" <> (showAsText . dominance $ card)
                   <> "&cardType=" <> (showAsText . cardType $ card)
                   <> "&subType=" <> (subType card)
                   <> "&gene1=" <> (showAsText . gene1 $ card)
                   <> "&gene2=" <> (showAsText . gene2 $ card)
                   <> "&illustration=" <> (illustration card)
                   <> "&startCards=" <> (showAsText . startCards $ card)
                   <> "&rules=" <> (rules card)
                   <> "&flavor=" <> (flavor card)
                   <> "&designer=" <> (designer card)
                  ), class_ "button"] "Edit"
      div_ [class_ "preview randomcolor"] $ do
        --mapM_ (\card -> p_ [] (renderCard NoLink card)) cards -- This renders all the versions
        renderCard NoLink card

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
    UnderConstruction -> do
      p_ [] (renderCardUnderConstruction card)
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
flavorText card = span_ [class_ "flavor"] $ toHtml (flavor card)

renderCardUnderConstruction :: Card -> Html ()
renderCardUnderConstruction card =
  do div_ [class_ "card ting"] $ do
       div_ [class_ "carddraw"] $ do
         span_ $ toHtml (show (startCards card))
       div_ [class_ "title"] $ toHtml (title card)
       illustrationDiv card
       typesDivWithDomination "ting" (subType card) (dominance card)
       div_ [class_ "ability"] $ do
         span_ [class_ "rules"] $ toHtml (rules card)
         br_ []
         flavorText card
       span_ [class_ "designer"] $ toHtml ("✏️ " <> (designer card))
       div_ [class_ ("gene2 " <> (showAsText . gene2) card)] $ return ()
       div_ [class_ ("gene1 " <> (showAsText . gene1) card)] $ return ()
       span_ [class_ "cardType"] (toHtml "ting")
       span_ [class_ "genes"] (toHtml ((showAsText . gene1) card <> " " <> (showAsText . gene2) card))

renderTing :: Card -> Html ()
renderTing card =
  do div_ [class_ "card ting"] $ do
       div_ [class_ "title"] $ toHtml (title card)
       illustrationDiv card
       typesDivWithDomination "ting" (subType card) (dominance card)
       div_ [class_ "ability"] $ do
         span_ [class_ "rules"] $ toHtml (rules card)
         br_ []
         flavorText card
       span_ [class_ "designer"] $ toHtml ("✏️ " <> (designer card))
       div_ [class_ ("gene2 " <> (showAsText . gene2) card)] $ return ()
       div_ [class_ ("gene1 " <> (showAsText . gene1) card)] $ return ()
       span_ [class_ "cardType"] (toHtml "ting")
       span_ [class_ "genes"] (toHtml (pack (show (gene1 card)) <> " " <> pack (show (gene2 card))))

renderEvent :: Card -> Html ()
renderEvent card =
  do div_ [class_ "card event"] $ do
       div_ [class_ "title"] $ toHtml (title card)
       illustrationDiv card
       typesDiv "event" (subType card)
       div_ [class_ "ability"] $ do
         span_ [class_ "rules"] $ toHtml (rules card)
         br_ []
         flavorText card
       span_ [class_ "designer"] $ toHtml ("✏️  " <> (designer card))
       div_ [class_ ("gene2 " <> (showAsText . gene2) card)] $ return ()
       div_ [class_ ("gene1 " <> (showAsText . gene1) card)] $ return ()
       span_ [class_ "cardType"] (toHtml "event")
       span_ [class_ "genes"] (toHtml (pack (show (gene1 card)) <> " " <> pack (show (gene2 card))))
       
renderBiom :: Card -> Html ()
renderBiom card =
  do div_ [class_ "card biom"] $ do
       div_ [class_ "title"] $ toHtml (title card)
       illustrationDiv card
       typesDivWithDomination "biom" (subType card) (dominance card)
       div_ [class_ "ability"] $ do
         span_ [class_ "rules"] $ toHtml (rules card)
       span_ [class_ "designer"] $ toHtml ("✏️  " <> (designer card))
       span_ [class_ "cardType"] (toHtml "biom")

renderMutation :: Card -> Html ()
renderMutation card =
  do div_ [class_ "card mutation"] $ do
       illustrationDiv card
       typesDiv "mutation" (subType card)
       div_ [class_ "title"] $ toHtml (title card)
       div_ [class_ "ability"] $ do
         span_ [class_ "rules"] $ toHtml (rules card)
         br_ []
         flavorText card
       span_ [class_ "designer"] $ toHtml ("✏️  " <> (designer card))
       div_ [class_ ("gene2 " <> (showAsText . gene2) card)] $ return ()
       div_ [class_ ("gene1 " <> (showAsText . gene1) card)] $ return ()
       span_ [class_ "cardType"] (toHtml "mutation")
       span_ [class_ "genes"] (toHtml (pack (show (gene1 card)) <> " " <> pack (show (gene2 card))))

renderSplicer :: Card -> Html ()
renderSplicer card =
  do div_ [class_ "card splicer"] $ do
       div_ [class_ "carddraw"] $ do
         span_ $ toHtml (show (startCards card))
       div_ [class_ "title"] $ toHtml (title card)
       illustrationDiv card
       typesDiv "splicer" (subType card)
       div_ [class_ "ability"] $ do
         span_ [class_ "rules"] $ toHtml (rules card)
         br_ []
         flavorText card
       span_ [class_ "designer"] $ toHtml ("✏️  " <> (designer card))
       span_ [class_ "cardType"] (toHtml "splicer")

field :: Text -> Text -> Text -> Text -> Text -> Html ()
field name heading helpText inputType defaultValue =
  div_ [class_ "inputField"] $ do
    span_ (toHtml (heading <> " "))
    a_ [href_ ("#" <> name)] (toHtml "?")
    br_ []
    input_ [ type_ inputType, name_ name, value_ defaultValue]

textarea :: Text -> Text -> Text -> Text -> Html ()
textarea name heading helpText defaultValue =
  div_ [class_ "inputField"] $ do
    span_ (toHtml (heading <> " "))
    a_ [href_ ("#" <> name)] (toHtml "?")
    br_ []
    textarea_[ name_ name, rows_ "5"] $ do toHtml defaultValue

renderCardDesignForm :: Maybe Deck -> Card -> Text -> Text -> Html ()
renderCardDesignForm activeDeck copiedCard designGuidelines username  =
  renderPage "Add Card" activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
        h1_ [class_ "randomcolor"] (toHtml "Create a card")
        form_ [action_ "/submit-card"] $ do
          div_ [class_ "inputField"] $ do
            span_ [style_ "white-space: pre;"] (toHtml "Select a card type: ")

            select_ [name_ "cardType"] $ do
              let cardTypeText = (showAsText . cardType $ copiedCard)
              option_ [value_ cardTypeText, selected_ "", hidden_ ""] (toHtml cardTypeText)
              option_ [value_ "Ting"] (toHtml "Ting")
              option_ [value_ "Biom"] (toHtml "Biom")
              option_ [value_ "Event"] (toHtml "Event")
              option_ [value_ "Mutation"] (toHtml "Mutation")
              option_ [value_ "Splicer"] (toHtml "Splicer")
            a_ [href_ "#cardtypes"] (toHtml "?")
          field "title" "Title" "" "text" (title copiedCard)
          field "subtype" "Subtype" " (i.e. Animal, Plant...)" "text" (subType copiedCard)
          field "dominance" "Dominance" " (0-10)" "number" ((showAsText) (dominance copiedCard))

          div_ [id_ "genes", class_ "inputField"] $ do
            span_ (toHtml "Select genes: ")
            a_ [href_ "#genes"] (toHtml "?")
            br_ []
            select_ [name_ "gene1"] $ do
              let geneText = (showAsText . gene1 $ copiedCard)
              option_ [value_ geneText , selected_ "", hidden_ ""] (toHtml geneText)
              option_ [value_ "NoGene"] (toHtml "NoGene")
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
              let geneText = (showAsText . gene2 $ copiedCard)
              option_ [value_ geneText , selected_ "", hidden_ ""] (toHtml geneText)
              option_ [value_ "NoGene"] (toHtml "NoGene")
              option_ [value_ "Air"] (toHtml "Air")
              option_ [value_ "Artificial"] (toHtml "Artificial")
              option_ [value_ "Bug"] (toHtml "Bug")
              option_ [value_ "Fungi"] (toHtml "Fungi")
              option_ [value_ "Mini"] (toHtml "Mini")
              option_ [value_ "Plant"] (toHtml "Plant")
              option_ [value_ "Nautic"] (toHtml "Nautic")
              option_ [value_ "Sinister"] (toHtml "Sinister")
              option_ [value_ "Land"] (toHtml "Land")

          field "startcards" "Start cards" " (The number of cards you start with)" "number" ((showAsText . startCards) copiedCard)
          textarea "rules" "Rules" " (example: @: Roam)" (rules copiedCard)
          textarea "flavor" "Flavor" "" (flavor copiedCard)
          input_ [type_ "text", name_ "designer", value_ username, readonly_ ""]
          field "illustration" "Illustration" " (URL)" "text" (illustration copiedCard)
          br_ []
          input_ [class_ "button", type_ "submit", value_ "Submit"]
      div_ [class_ "preview randomcolor"] $ do
        renderCard UnderConstruction (copiedCard { designer = username })
    br_ []
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
        article_ [class_ "markdown"] (toHtml designGuidelines)

renderSubmittedCard :: Maybe Deck -> Text -> Html ()
renderSubmittedCard activeDeck title =
  renderPage title activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
        p_ $ span_ [] $ do (toHtml "The card ")
                           a_ [href_ ("/card/" <> title)] (toHtml title)
                           (toHtml " was added. Thank you!")
        a_ [href_ "/cards", class_ "button"] "See all cards"
        a_ [href_ "/add-card", class_ "button"] "Add another card"

renderPlayerPage :: Maybe Deck -> Text -> [Text] -> [Deck] -> Html ()
renderPlayerPage activeDeck username myCardTitles myDecks = do
  renderPage username activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do

        h1_ [class_ "randomcolor"] (toHtml username)
        a_ [href_ "/new-deck", class_ "button"] "Create a new deck"
        a_ [href_ "/add-card", class_ "button"] "Create a new card"
        a_ [href_ "/logout", class_ "button"] "Log out"
        h3_ "Decks by me"
        mapM_ (\(deck) -> li_ $ do a_ [href_ ("/deck/" <> (showAsText . deckId) deck)] (toHtml $ deckName deck)
                                   span_ [] (toHtml " ")
                                   let onClickCode = "deleteDeck(" <> (showAsText) (deckId deck) <> ")"
                                   a_ [onclick_ onClickCode, class_ "subtle-button"] (toHtml "✖"))
          myDecks

        h3_ "Cards by me"
        mapM_ (\cardTitle -> li_ $ a_ [href_ $ pack ("/card/" ++ unpack cardTitle)] (toHtml cardTitle)) myCardTitles

-- There is a reason for the two deck arguments!
-- 'activeDeck' is the deck that the player is currently editing, its ID is stored in a cookie for the session
-- 'deck' is the deck at the page that the player has navigated to, i.e. /decks/123
renderDeckPage :: Maybe Deck -> Deck -> [Card] -> Html ()
renderDeckPage activeDeck deck cards = do
  renderPage "Deck" activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content deckedit"] $ do
        -- TODO: Move this CSS to the stylesheets?
        div_ [class_ "randomcolor", style_ "margin-left: -20px; margin-right: -20px;"] $ do
          a_ [class_ "whileediting button", style_ "display:none;",  href_ "#", onclick_ "removeCookie()"] "Finish"
          a_ [class_ "notediting button",  href_ "#", onclick_ "editDeck()"] "Edit"
          a_ [class_ "button",  href_ ("/print/" <> (showAsText . deckId) deck), target_ "new"] "Print"
          a_ [class_ "notediting button", onclick_ ("deleteDeck(" <> ((showAsText . deckId) deck) <> ")")] "Delete"
          input_ [id_ "deckname", value_ (deckName deck), name_ "deckname"]
        div_ [ class_ "deckstatus randomcolor"] $ do
          span_ [class_ "deckcounter"] (toHtml "number of cards in deck")
          span_ [class_ "whileediting", style_ "display:none;"] (toHtml "Currently editing.  Click a card to remove it from your deck.")
        input_ [name_ "deckid", style_ "display:none", value_ ((showAsText . deckId) deck), readonly_ ""]

        a_ [class_ "whileediting", style_ "display:none;", href_ "/cards"] $ do
                                                                     div_ [class_ "add"] $ do
                                                                       span_ [] (toHtml "Add more cards to deck")
        inDeckSelection <- case activeDeck of
                             Just d -> return ((deckId d) == (deckId deck))
                             Nothing -> return False

        if inDeckSelection then do
          mapM_ (renderCard InDeckSelection) cards
        else
          mapM_ (renderCard AsLink) cards

renderPrintDeckPage :: Maybe Deck -> Deck -> [Card] -> Html ()
renderPrintDeckPage _ deck cards = do
  renderPage "Print Deck" (Just deck) $ do
        script_ "document.getElementsByTagName('link')[0].disabled = true; $('.randomcolor').css('background-color', 'none');"
        mapM_ (renderCard NoLink) cards
        script_ "window.print();"

renderNoSuchDeckPage :: Maybe Deck -> Html ()
renderNoSuchDeckPage activeDeck = do
  renderPage "No such deck" activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
        p_ (toHtml "No deck with that id was found.")

renderSignupForm :: Maybe Deck -> Html ()
renderSignupForm activeDeck = do
  renderPage "Sign Up" activeDeck $ do
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
  renderPage "Failed to sign up" activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
        h1_ "Failed to sign up"
        p_ "Perhaps that username is taken."

renderLoginFormFull :: Text -> Maybe Deck -> Html ()
renderLoginFormFull nextPage activeDeck = do
  renderPage "Login" activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
          renderLoginForm nextPage

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
  renderPage "Must Login" activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
        p_ (toHtml helpText)
        renderLoginForm nextPage
        p_ [] "Don't have a player account?"
        a_ [href_ "/signup", class_ "button"] "Sign up here!"

renderSucceededToLogin :: Maybe Deck -> Html ()
renderSucceededToLogin activeDeck = do
  renderPage "Logged in!" activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
          p_ "You are logged in!"

renderFailedToLogin :: Maybe Deck -> Html ()
renderFailedToLogin activeDeck = do
  renderPage "Failed to log in..." activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
          p_ "Failed to login, invalid password or username."

renderLogout :: Maybe Deck -> Html ()
renderLogout activeDeck = do
  renderPage "Logged out" activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
          p_ "You have been logged out."
          a_ [href_ "/login?next=player", class_ "button"] "Login"

renderKeywordPage :: Maybe Deck -> [Keyword] -> Html ()
renderKeywordPage activeDeck keywords = do
  renderPage "Keywords" activeDeck $ do
    mapM_ renderKeyword keywords

renderKeyword :: Keyword -> Html ()
renderKeyword kw = do h2_ (toHtml (keywordName kw))
                      p_ (toHtml (keywordRules kw))

renderRulesDocument :: Maybe Deck -> Text -> Html ()
renderRulesDocument activeDeck rulesText = do
  renderPage "Rules" activeDeck $ do
    div_ [id_ "page"] $ do
      renderMenu
      div_ [] $ do
        article_ [class_ "markdown"] (toHtml rulesText)

renderMenu :: Html ()
renderMenu = do
      div_ [id_ "logo", class_ "randomcolor"] $ do
        img_ [src_ "/files/logo.png", id_ "logobg"]
        img_ [src_ "/files/logo_card.png", id_ "spinner"]
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
  renderPage "Error" activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
        p_ (toHtml message)
