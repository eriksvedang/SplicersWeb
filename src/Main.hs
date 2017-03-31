{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Web.Spock.Core
import Web.PathPieces (PathPiece)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Environment (getArgs)
import System.Environment (lookupEnv)
import Data.Text (unpack, pack, strip, append, Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Monoid ((<>))
import Data.List (nub)
import Data.Maybe (maybe, isJust)
import qualified Crypto.BCrypt as BC
import Database
import Card
import Rendering
import Lucid
import Player
import Deck
import Util

type Route = ActionT IO ()

main :: IO ()
main = do
  migrate
  port <- fmap read getPort
  runSpock port $ spockT id $ do
    -- Main routes
    get root $                     frontPageRoute
    get "rules" $                  rulesDocumentRoute
    get ("files" <//> var)         getFile

    -- Player routes
    get "signup" $                 signupRoute
    get "submit-signup" $          submitSignupRoute
    get "fail-signup" $            failSignupRoute
    get "login" $                  loginRoute
    get "submit-login" $           submitLoginRoute
    get "logout" $                 logoutRoute
    get "player" $                 userPageRoute
    get "user" $                   userPageRoute -- alias for /player

    -- Card routes
    get "cards" $                  cardsRoute
    get ("card" <//> var) $        singleCardRoute
    get "add-card" $               cardDesignerRoute
    get "submit-card" $            submitCardRoute

    -- Deck routes
    get ("deck" <//> var) $        deckRoute
    get ("print" <//> var) $       printDeckRoute
    get ("edit-deck" <//> var) $   editDeckRoute
    get "set-deck-name" $          setDeckNameRoute
    get "new-deck" $               newDeckRoute
    get "delete-deck" $            deleteDeckRoute
    get "add-card-to-deck" $       addCardToDeckRoute
    get "remove-card-from-deck" $  removeCardFromDeckRoute

    -- Keyword routes
    get "keywords" $               listKeywordsRoute

frontPageRoute :: Route
frontPageRoute = do
  activeDeck <- getActiveDeck
  lucidToSpock $ renderFrontPage activeDeck

getDeckId :: ActionT IO Int
getDeckId = do
  deckIdAsText <- cookie "deck"
  let deckIdAsInt = case deckIdAsText of
                      (Just deckId) -> case reads (unpack deckId) of
                                         [] -> 0
                                         [(n, _)] -> n
                      Nothing -> 0
  return deckIdAsInt

getActiveDeck :: ActionT IO (Maybe Deck)
getActiveDeck = do
  deckId <- getDeckId
  deckToEdit <- liftIO $ getDeck deckId
  return deckToEdit

cardsRoute :: Route
cardsRoute = do
  cards <- liftIO getCards
  deckId <- getDeckId
  activeDeck <- getActiveDeck
  cardTitles <- liftIO $ getCardsInDeck deckId
  cardsInDeck <- liftIO $ mapM getNewestCardWithTitle cardTitles
  lucidToSpock $ renderCards cards activeDeck cardsInDeck

singleCardRoute :: Text -> Route
singleCardRoute title = do
  cards <- liftIO (getCardsWithTitle title)
  activeDeck <- getActiveDeck
  lucidToSpock (renderSingleCardPage activeDeck title cards)

-- This is just for rendering the page for designing cards, NOT adding them to the DB.
cardDesignerRoute :: Route
cardDesignerRoute = do
  activeDeck <- getActiveDeck
  cardTitle <- paramOrDefault "title" "untitled"
  rules <- paramOrDefault "rules" ""
  domination <- paramOrDefault "domination" "0"
  cardType <- paramOrDefault "cardType" "Ting"
  subType <- paramOrDefault "subType" ""
  gene1 <- paramOrDefault "gene1" ""
  gene2 <- paramOrDefault "gene2" ""
  startMatter <- paramOrDefault "startMatter" "0"
  startCards <- paramOrDefault "startCards" "0"
  flavor <- paramOrDefault "flavor" ""
  theDesigner <- paramOrDefault "designer" "unknown"
  illustration <- paramOrDefault "illustration" ""
  designGuidelines <- liftIO $ readFile "./files/designGuidelines.md"
  let copiedCard  = mkCard (strip cardTitle)
                         rules
                         (read domination)
                         cardType
                         (strip subType)
                         gene1
                         gene2
                         (read startMatter)
                         (read startCards)
                         flavor
                         theDesigner
                         illustration
  withAuth (renderCardDesignForm activeDeck copiedCard (pack designGuidelines)) "add-card"

paramOrDefault :: (PathPiece p, MonadIO m) => Text -> p -> ActionT m p
paramOrDefault name defaultValue = do
  maybeValue <- param name
  case maybeValue of -- TODO: Use Data.Maybe.maybe here
    Just value -> return value
    Nothing -> return defaultValue

submitCardRoute :: Route
submitCardRoute = withAuthImproved "/add-card" $ do
  activeDeck <- getActiveDeck
  cardTitle <- paramOrDefault "title" "untitled"
  rules <- paramOrDefault "rules" ""
  dominance <- paramOrDefault "dominance" "0"
  cardType <- paramOrDefault "cardType" ""
  subType <- paramOrDefault "subtype" ""
  gene1 <- paramOrDefault "gene1" "NoGene"
  gene2 <- paramOrDefault "gene2" "NoGene"
  startMatter <- paramOrDefault "startMatter" "0"
  startCards <- paramOrDefault "startcards" "0"
  flavor <- paramOrDefault "flavor" ""
  theDesigner <- paramOrDefault "designer" "unknown"
  illustration <- paramOrDefault "illustration" ""

  previousVersions <- liftIO (getCardsWithTitle cardTitle)
  let someoneElseOwnsThisCard = 0 < (length (filter (/= theDesigner) (map designer previousVersions)))

  if someoneElseOwnsThisCard then
    lucidToSpock (renderError activeDeck "Someone else owns this card (try using another title).")
  else do
    let card = mkCard (strip cardTitle)
                      rules
                      (read dominance)
                      cardType
                      (strip subType)
                      gene1
                      gene2
                      (read startMatter)
                      (read startCards)
                      flavor
                      theDesigner
                      illustration
    case verifyCard card of
      Left msg -> lucidToSpock (renderError activeDeck msg)
      Right _  -> do liftIO (addCard card)
                     lucidToSpock (renderSubmittedCard activeDeck cardTitle)

signupRoute :: Route
signupRoute = do
  lucidToSpock (renderSignupForm Nothing)

submitSignupRoute :: Route
submitSignupRoute = do
  Just username <- param "username"
  Just email    <- param "email"
  Just password <- param "password"
  Just salt <- liftIO $ BC.genSaltUsingPolicy BC.fastBcryptHashingPolicy
  hashed <- liftIO $ hashPlainPassword password (decodeUtf8 salt)
  success <- liftIO $ addPlayer (Player { playerName = username,
                                          playerEmail = email,
                                          playerPassword = hashed,
                                          playerSalt = (decodeUtf8 salt) })
  if success then do
    setCookie "username" username defaultCookieSettings
    redirect "/player"
  else
    redirect "/fail-signup"

failSignupRoute :: Route
failSignupRoute = do
  lucidToSpock $ renderFailSignup Nothing

loginRoute :: Route
loginRoute = do
  maybeNextPage <- param "next"
  let nextPage = case maybeNextPage of
                   Just x -> x
                   Nothing -> ""
  lucidToSpock $ renderLoginFormFull nextPage Nothing

submitLoginRoute = do
  Just username <- param "username"
  Just password <- param "password"
  maybeSecret <- liftIO $ authorize username password
  case maybeSecret of
    Just secret -> do
      setCookie "username" username defaultCookieSettings
      setCookie "secret" secret defaultCookieSettings
      maybeNextPage <- param "next"
      case maybeNextPage of
        Just nextPage | not ("" == nextPage) -> redirect $ nextPage
                      | otherwise -> lucidToSpock $ renderSucceededToLogin Nothing
        Nothing -> lucidToSpock $ renderSucceededToLogin Nothing
    Nothing -> do
      lucidToSpock $ renderFailedToLogin Nothing

logoutRoute = do
  deleteCookie "username"
  deleteCookie "secret"
  deleteCookie "deck"
  lucidToSpock $ renderLogout Nothing

userPageRoute :: Route
userPageRoute = do
  maybeName <- cookie "username"
  let name = case maybeName of
               Just n -> n
               Nothing -> ""
  myCards <- liftIO $ getCardsByDesigner name
  myDecks <- liftIO $ getDecks name
  activeDeck <- getActiveDeck
  withAuth (\username -> renderPlayerPage activeDeck username (nub (fmap title myCards)) myDecks) "player"

type DeckRenderer = Maybe Deck -> Deck -> [Card] -> Html ()

-- TODO: pointfree style
deckRoute :: Text -> Route
deckRoute deckId = generalizedDeckRenderingRoute renderDeckPage deckId

printDeckRoute :: Text -> Route
printDeckRoute deckId = generalizedDeckRenderingRoute renderPrintDeckPage deckId

generalizedDeckRenderingRoute :: DeckRenderer -> Text -> Route
generalizedDeckRenderingRoute renderer deckId = do
  let deckIdAsInt = readText deckId
  deck <- liftIO $ getDeck deckIdAsInt
  cardTitles <- liftIO $ getCardsInDeck deckIdAsInt
  cards <- liftIO $ mapM getNewestCardWithTitle cardTitles
  activeDeck <- getActiveDeck
  case deck of
    (Just deck) -> lucidToSpock $ renderer activeDeck deck cards
    Nothing -> lucidToSpock $ renderNoSuchDeckPage activeDeck

editDeckRoute :: Text -> Route
editDeckRoute deckId = do
  setCookie "deck" deckId defaultCookieSettings
  let page = (append "/deck/" deckId)
  redirect page

setDeckNameRoute :: Route
setDeckNameRoute = withAuthImproved "/player" $ do
  Just deckIdStr <- param "deckId"
  Just deckName <- param "deckName"
  let deckId = readText deckIdStr
  deck <- liftIO $ getDeck deckId
  activeDeck <- getActiveDeck
  case deck of
    Just deck -> do Just username <- cookie "username" -- should be safe since the auth was OK
                    if username == (deckDesigner deck)
                      then do liftIO $ setDeckName deckId deckName
                              lucidToSpock (p_ [] "Deck name was set.")
                      else lucidToSpock (renderError activeDeck "Can't set deck name of someone else's deck.")    

newDeckRoute :: Route
newDeckRoute = withAuthImproved "/new-deck" $ do
  username <- cookie "username"
  case username of
    Just name -> do newDeckId <- liftIO $ addDeck (Deck 0 "Awesome New Deck" name)
                    setCookie "deck" (showAsText newDeckId) defaultCookieSettings
                    redirect "/cards"
    Nothing -> error "Can't create deck when not logged in."

deleteDeckRoute :: Route
deleteDeckRoute = withAuthImproved "/player" $ do
  username <- cookie "username"
  activeDeck <- getActiveDeck
  deleteCookie "deck"
  case username of
    Just name -> do deckIdStr <- param "deckId"
                    case deckIdStr of
                      Just s  -> do liftIO $ deleteDeck (readText s)
                                    setCookie "deck" "" defaultCookieSettings
                                    redirect "/player"
                      Nothing -> lucidToSpock $ renderError activeDeck "Missing parameter: 'deckId'"
    Nothing -> error "Can't delete deck when not logged in."

addCardToDeckRoute :: Route
addCardToDeckRoute = withAuthImproved "/" $ do
  Just deckId <- param "deckId"
  Just cardTitle <- param "cardTitle"
  liftIO $ addCardToDeck (readText deckId) cardTitle
  lucidToSpock (p_ [] "Card added.")

removeCardFromDeckRoute :: Route
removeCardFromDeckRoute = withAuthImproved "/" $ do
  Just deckId <- param "deckId"
  Just cardTitle <- param "cardTitle"
  liftIO $ removeCardFromDeck (readText deckId) cardTitle
  lucidToSpock (p_ [] "Card removed.")

listKeywordsRoute :: Route
listKeywordsRoute = do
  keywords <- liftIO getKeywords
  activeDeck <- getActiveDeck
  lucidToSpock $ renderKeywordPage activeDeck keywords

rulesDocumentRoute :: Route
rulesDocumentRoute = do
  doc <- liftIO $ readFile "./files/rules.md"
  activeDeck <- getActiveDeck
  lucidToSpock $ renderRulesDocument activeDeck (pack doc)

getFile :: String -> Route
getFile name = file (pack name) ("./files/" ++ name)

lucidToSpock :: Html () -> Route
lucidToSpock t = (html . toStrict . renderText) t

defaultPort :: String
defaultPort = "8080"

getPort :: IO String
getPort = do
  maybePort <- lookupEnv "PORT"
  case maybePort of
    (Just port) -> return port
    Nothing -> do
      putStrLn $ "No PORT string found in environment, using default (" ++ defaultPort ++ ")."
      return defaultPort

isAuthorized :: ActionT IO Bool
isAuthorized = fmap isJust (cookie "username")

-- TODO: Just use one of the withAuth functions!

withAuth :: (Text -> Html ()) -> Text -> ActionT IO ()
withAuth authenticatedRoute goHereAfterLogin = do
  maybeUsername <- cookie "username"
  activeDeck <- getActiveDeck
  case maybeUsername of
    Just username -> lucidToSpock (authenticatedRoute username)
    Nothing -> lucidToSpock $ renderMustLogIn activeDeck "Please log in first." goHereAfterLogin

withAuthImproved :: Text -> Route ->  ActionT IO ()
withAuthImproved goHereAfterLogin authenticatedRoute = do
  maybeUsername <- cookie "username"
  activeDeck <- getActiveDeck
  case maybeUsername of
    Just username -> authenticatedRoute
    Nothing -> lucidToSpock $ renderMustLogIn activeDeck "Please log in first." goHereAfterLogin
