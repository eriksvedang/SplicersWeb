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

renderPage :: Maybe Deck -> Html () -> Html ()
renderPage activeDeck body = do head_ $ do
                                  (link_ [rel_ "icon", type_ "image/png", href_ "/files/favicon.png"])
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
         article_ $ do
           img_ [src_ "/files/splash.jpg", class_ "splash"]
           p_ "Have you ever played a collectible card game, like Magic the Gathering or Pokémon? They're awesome but they have two big flaws (if you ask us!). First of all they cost a lot of money to play, since all the cards have to be bought in small booster packs or from people who have made it their business to trade these cards. Secondly these games are made by a small group of people who curates the pool of cards. This is obviously great for the quality of the game - they work with this all day long after all - but it also means that a lot of creativity is wasted. YOUR creativity that is."
           p_ "This is where Splicers comes in! It is a collectible card game where everyone is allowed to contribute their ideas for cards. Anything goes, although there are also guidelines to keep the game somewhat cohesive and understandable. If you don't want to come up with cards, that's cool too. You can just create decks with the existing cards and play against others. This website makes it super easy to manage your deck designs and print them out. You'll have to do some cutting but that's it (and the decks are really small)."

cornerWidget :: Maybe Deck -> Html ()
cornerWidget deckToEdit = do
  div_ [id_ "filter", class_ "randomcolor", style_ "display: none;"] $ do
    span_ [] (toHtml "Filter cards →")
    select_ [ type_ "text", name_ "filterType"] $ do
      option_ [ value_ "title" ] (toHtml "Title")
      option_ [ value_ "cardType" ] (toHtml "Card Type")
      option_ [ value_ "types" ] (toHtml "Subtype")
      option_ [ value_ "genes" ] (toHtml "Genes")
      option_ [ value_ "ability" ] (toHtml "Ability Text")
      option_ [ value_ "designer" ] (toHtml "Designer")
    span_ [] (toHtml "→")
    input_ [ type_ "text", name_ "filter"]
  case deckToEdit of
    Just deck -> a_ [class_ "editing randomcolor", href_ (T.append "/deck/" ((pack . show . deckId) deck))]
                 (toHtml (T.append "Editing: " (deckName deck)))
    Nothing -> span_ [class_ "editing"] "No deck to edit"


renderCards :: [Card] -> Maybe Deck -> [Card] -> Html ()
renderCards cards activeDeck cardsInDeck = renderPage activeDeck $
  do a_ [ href_ "add-card"] $ do
       div_ [class_ "add"] $ do
         span_ [] (toHtml "+ Create a card")
     mapM_ (\card -> renderCard (if ((title card) `elem` (map title cardsInDeck)) then InDeckSelection else AsLink) card) cards


data RenderCardMode = AsLink | NoLink | InDeckSelection | UnderConstruction

renderSingleCardPage :: Maybe Deck -> Text -> [Card] -> Html ()
renderSingleCardPage activeDeck title cards =
  renderPage activeDeck $ do
    div_ [class_ "window"] $ do
      let card = head cards
      div_ [class_ "content"] $ do
        h1_ [] (toHtml title)
        span_ [] (toHtml $ "Designed by " <> (designer (head cards)))
        a_ [href_ ("/add-card/?title=" <> title
                   <> "&rules=" <> (rules card)
                   <> "&domination=" <> (pack . show . dominance $ card)
                   <> "&cardType=" <> (pack . show . cardType $ card)
                   <> "&subType=" <> (subType card)
                   <> "&gene1=" <> (pack . show . gene1 $ card)
                   <> "&gene2=" <> (pack . show . gene2 $ card)
                   <> "&illustration=" <> (illustration card)
                   <> "&startCards=" <> (pack . show . startCards $ card)
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
         div_ [class_ (T.append "gene1 " (pack (show (gene1 card))))] $ return ()
         div_ [class_ (T.append "gene2 " (pack (show (gene2 card))))] $ return ()
         span_ [class_ "rules"] $ toHtml (rules card)
         br_ []
         flavorText card
       span_ [class_ "designer"] $ toHtml ("designed by " <> (designer card))
       span_ [class_ "cardType"] (toHtml "ting")
       span_ [class_ "genes"] (toHtml (pack (show (gene1 card)) <> " " <> pack (show (gene2 card))))

renderTing :: Card -> Html ()
renderTing card =
  do div_ [class_ "card ting"] $ do
       div_ [class_ "title"] $ toHtml (title card)
       illustrationDiv card
       typesDivWithDomination "ting" (subType card) (dominance card)
       div_ [class_ "ability"] $ do
         div_ [class_ (T.append "gene1 " (pack (show (gene1 card))))] $ return ()
         div_ [class_ (T.append "gene2 " (pack (show (gene2 card))))] $ return ()
         span_ [class_ "rules"] $ toHtml (rules card)
         br_ []
         flavorText card
       span_ [class_ "designer"] $ toHtml ("designed by " <> (designer card))
       span_ [class_ "cardType"] (toHtml "ting")
       span_ [class_ "genes"] (toHtml (pack (show (gene1 card)) <> " " <> pack (show (gene2 card))))

renderEvent :: Card -> Html ()
renderEvent card =
  do div_ [class_ "card event"] $ do
       div_ [class_ "title"] $ toHtml (title card)
       illustrationDiv card
       typesDiv "event" (subType card)
       div_ [class_ "ability"] $ do
         div_ [class_ (T.append "gene1 " (pack (show (gene1 card))))] $ return ()
         div_ [class_ (T.append "gene2 " (pack (show (gene2 card))))] $ return ()
         span_ [class_ "rules"] $ toHtml (rules card)
         br_ []
         flavorText card
       span_ [class_ "designer"] $ toHtml ("designed by " <> (designer card))
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
         --flavorText card
       span_ [class_ "designer"] $ toHtml ("designed by " <> (designer card))
       span_ [class_ "cardType"] (toHtml "biom")
       
renderMutation :: Card -> Html ()
renderMutation card =
  do div_ [class_ "card mutation"] $ do
       illustrationDiv card
       typesDiv "mutation" (subType card)
       div_ [class_ "title"] $ toHtml (title card)
       div_ [class_ "ability"] $ do
         div_ [class_ (T.append "gene1 " (pack (show (gene1 card))))] $ return ()
         div_ [class_ (T.append "gene2 " (pack (show (gene2 card))))] $ return ()
         span_ [class_ "rules"] $ toHtml (rules card)
         br_ []
         flavorText card
       span_ [class_ "designer"] $ toHtml ("designed by " <> (designer card))
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
       span_ [class_ "designer"] $ toHtml ("designed by " <> (designer card))
       span_ [class_ "cardType"] (toHtml "splicer")
       
field :: Text -> Text -> Text -> Text -> Text -> Html ()
field name heading helpText inputType defaultValue =
  div_ [class_ "inputField"] $ do
    span_ (toHtml heading)
    a_ [href_ "#"] (toHtml "?")
    br_ []
    input_ [ type_ inputType, name_ name, value_ defaultValue]

textarea :: Text -> Text -> Text -> Text -> Html ()
textarea name heading helpText defaultValue =
  div_ [class_ "inputField"] $ do
    span_ (toHtml heading)
    a_ [href_ "#"] (toHtml "?")
    br_ []
    textarea_[ name_ name, rows_ "5"] $ do toHtml defaultValue


renderAddCard :: Maybe Deck -> Card -> Text -> Html ()
renderAddCard activeDeck copiedCard username =
  renderPage activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
        h1_ [class_ "randomcolor"] (toHtml "Create a card")
        form_ [action_ "/submit-card"] $ do
          div_ [class_ "inputField"] $ do
            span_ [style_ "white-space: pre;"] (toHtml "Select a card type: ")

            select_ [name_ "cardType"] $ do
              let cardTypeText = (pack . show . cardType $ copiedCard)
              option_ [value_ cardTypeText, selected_ "", hidden_ ""] (toHtml cardTypeText)
              option_ [value_ "Ting"] (toHtml "Ting")
              option_ [value_ "Biom"] (toHtml "Biom")
              option_ [value_ "Event"] (toHtml "Event")
              option_ [value_ "Mutation"] (toHtml "Mutation")
              option_ [value_ "Splicer"] (toHtml "Splicer")

          field "title" "Title" "" "text" (title copiedCard)
          field "subType" "Subtype" " (i.e. Animal, Plant...)" "text" (subType copiedCard)
          field "domination" "Dominance" " (0-10)" "number" ((pack . show) (dominance copiedCard))

          div_ [id_ "genes", class_ "inputField"] $ do
            span_ (toHtml "Select genes: ")
            br_ []
            select_ [name_ "gene1"] $ do
              let geneText = (pack . show . gene1 $ copiedCard)
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
              let geneText = (pack . show . gene2 $ copiedCard)
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

          field "startCards" "Cards" " (The number of cards you start with)" "number" ((pack . show . startCards) copiedCard)
          textarea "rules" "Rule text" " (example: @: Roam)" (rules copiedCard)
          textarea "flavor" "Flavour text" "" (flavor copiedCard)
          input_ [type_ "text", name_ "designer", value_ username, readonly_ ""]
          field "illustration" "Image" " (URL)" "text" (illustration copiedCard)
          br_ []
          input_ [class_ "button", type_ "submit", value_ "Submit"]
      div_ [class_ "preview randomcolor"] $ do
        renderCard UnderConstruction copiedCard

    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
        a_ [name_ "subtype"] "learn more"

renderSubmittedCard :: Maybe Deck -> Text -> Html ()
renderSubmittedCard activeDeck title =
  renderPage activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
        p_ $ span_ [] $ do (toHtml "The card ")
                           a_ [href_ ("/card/" <> title)] (toHtml title)
                           (toHtml " was added. Thank you!")
        a_ [href_ "/cards", class_ "button"] "See all cards"
        a_ [href_ "/add-card", class_ "button"] "Add another card"

renderAddFakeData :: Maybe Deck -> Html ()
renderAddFakeData activeDeck = do
  renderPage activeDeck $ p_ "Added fake data!"

renderPlayerPage :: Maybe Deck -> Text -> [Text] -> [Deck] -> Html ()
renderPlayerPage activeDeck username myCardTitles myDecks = do
  renderPage activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do

        h1_ [class_ "randomcolor"] (toHtml username)
        a_ [href_ "/new-deck", class_ "button"] "Create a new deck"
        a_ [href_ "/add-card", class_ "button"] "Create a new card"
        a_ [href_ "/logout", class_ "button"] "Log out"
        h3_ "Decks by me"
        mapM_ (\(deck) -> li_ $ do a_ [href_ $ pack ("/deck/" ++ show (deckId deck))] (toHtml $ deckName deck)
                                   span_ [] (toHtml " ")
                                   -- a_ [href_ $ T.append "/edit-deck/" ((pack . show . deckId) deck)] (toHtml "Edit")
                                   a_ [ href_ "", onclick_ ("deleteDeck(" <> (pack . show) (deckId deck) <> ")"), class_ "button" ] (toHtml "Delete"))
          myDecks

        h3_ "Cards by me"
        mapM_ (\cardTitle -> li_ $ a_ [href_ $ pack ("/card/" ++ unpack cardTitle)] (toHtml cardTitle)) myCardTitles



renderDeckPage :: Maybe Deck -> Deck -> [Card] -> Html ()
renderDeckPage activeDeck deck cards = do
  renderPage activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content deckedit"] $ do
        div_ [class_ "randomcolor", style_ "margin-left: -20px; margin-right: -20px;"] $ do
          a_ [class_ "whileediting button", style_ "display:none;",  href_ "#", onclick_ "removeCookie()"] "Finish"
          a_ [class_ "notediting button",  href_ "#", onclick_ "editDeck()"] "Edit"
          a_ [class_ "button",  href_ $ pack ("/print/" ++ show (deckId deck)), target_ "new"] "Print"
          a_ [class_ "notediting button", href_ "",  onclick_ ("deleteDeck(" <> ((pack . show . deckId) deck) <> ")")] "Delete"
          input_ [id_ "deckname", value_ (deckName deck), name_ "deckname"]

        input_ [name_ "deckid", style_ "display:none", value_ ((pack . show . deckId) deck), readonly_ ""]
        span_ [class_ "whileediting", style_ "display:none;"] (toHtml "Currently editing. Click a card to remove it from your deck. Click the title to change it. ")

        a_ [class_ "whileediting", style_ "display:none;", href_ "/cards"] $ do
                                                                     div_ [class_ "add"] $ do
                                                                       span_ [] (toHtml "Add more cards to deck")
        case activeDeck of
          Just deck -> mapM_ (renderCard InDeckSelection) cards
          Nothing -> mapM_ (renderCard AsLink) cards

renderPrintDeckPage :: Maybe Deck -> Deck -> [Card] -> Html ()
renderPrintDeckPage activeDeck deck cards = do
  renderPage activeDeck $ do
        script_ "document.getElementsByTagName('link')[0].disabled = true; $('.randomcolor').css('background-color', 'none');"
        case activeDeck of
          Just deck -> mapM_ (renderCard NoLink) cards
          Nothing -> mapM_ (renderCard NoLink) cards
        script_ "window.print();"
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

renderLoginFormFull :: Text -> Maybe Deck -> Html ()
renderLoginFormFull nextPage activeDeck = do
  renderPage activeDeck $ do
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
          a_ [href_ "/login?next=player", class_ "button"] "Login"

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
  renderPage activeDeck $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
        p_ (toHtml message)
