{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Web.Spock.Core
import Web.PathPieces (PathPiece)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Environment (getArgs)
import System.Environment (lookupEnv)
import Data.Text (unpack, pack)
import Data.Text.Internal (Text)
import Data.Text.Lazy (toStrict)
import Data.Monoid ((<>))
import Database (migrate, getCards, addCard, addFakeData, authorize, getSecret, getCardsWithTitle, getCardsByDesigner, addPlayer, getKeywords)
import Card
import Rendering
import Lucid
import Player

type Route = ActionT IO ()

main :: IO ()
main = do
  migrate
  port <- getPort
  runSpock (read port) $ spockT id $ do
    get root $              frontPageRoute
    get "cards" $           cardsRoute
    get ("card" <//> var) $ singleCardRoute
    get "add-card" $        addCardRoute
    get "submit-card" $     submitCardRoute
    get "add-fake-data" $   addFakeDataRoute
    get "signup" $          signupRoute
    get "submit-signup" $   submitSignupRoute
    get "fail-signup" $     failSignupRoute
    get "login" $           loginRoute
    get "submit-login" $    submitLoginRoute
    get "logout" $          logoutRoute
    get "user" $            userPageRoute
    get "keywords" $        listKeywordsRoute
    get ("files" <//> var)  getFile

frontPageRoute :: Route
frontPageRoute = do
  lucidToSpock renderFrontPage

cardsRoute :: Route
cardsRoute = do
  cards <- liftIO getCards
  lucidToSpock $ renderCards cards

singleCardRoute :: Text -> Route
singleCardRoute title = do
  cards <- liftIO (getCardsWithTitle title)
  lucidToSpock (renderSingleCardPage title cards)

addCardRoute :: Route
addCardRoute = do
  withAuth renderAddCard "add-card"

paramOrDefault :: (PathPiece p, MonadIO m) => Text -> p -> ActionT m p
paramOrDefault name defaultValue = do
  maybeValue <- param name
  case maybeValue of
    Just value -> return value
    Nothing -> return defaultValue

submitCardRoute :: Route
submitCardRoute = do
  title <- paramOrDefault "title" "untitled"
  rules <- paramOrDefault "rules" ""
  domination <- paramOrDefault "domination" "0"
  cardType <- paramOrDefault "cardType" ""
  subType <- paramOrDefault "subType" ""
  gene1 <- paramOrDefault "gene1" ""
  gene2 <- paramOrDefault "gene2" ""
  startMatter <- paramOrDefault "startMatter" "0"
  startCards <- paramOrDefault "startCards" "0"
  flavor <- paramOrDefault "flavor" ""
  designer <- paramOrDefault "designer" "unknown"
  illustration <- paramOrDefault "illustration" ""
  let card = mkCard title
                    rules
                    (read domination)
                    cardType
                    subType
                    gene1
                    gene2
                    (read startMatter)
                    (read startCards)
                    flavor
                    designer
                    illustration
  liftIO (addCard card)
  lucidToSpock (renderSubmittedCard title)

addFakeDataRoute :: Route
addFakeDataRoute = do
  liftIO addFakeData
  lucidToSpock renderAddFakeData

signupRoute :: Route
signupRoute = do
  lucidToSpock renderSignupForm

submitSignupRoute :: Route
submitSignupRoute = do
  Just username <- param "username"
  Just email    <- param "email"
  Just password <- param "password"
  success <- liftIO $ addPlayer (Player username email password)
  if success then do
    setCookie "username" username defaultCookieSettings
    redirect "/user"
  else
    redirect "/fail-signup"

failSignupRoute :: Route
failSignupRoute = do
  lucidToSpock renderFailSignup

loginRoute :: Route
loginRoute = do
  lucidToSpock renderLoginFormFull

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
        Just nextPage -> redirect $ "/" <> nextPage
        Nothing -> lucidToSpock renderSucceededToLogin
    Nothing -> do
      lucidToSpock renderFailedToLogin

logoutRoute = do
  deleteCookie "username"
  deleteCookie "secret"
  lucidToSpock renderLogout

userPageRoute :: Route
userPageRoute = do
  maybeName <- cookie "username"
  let name = case maybeName of
               Just n -> n
               Nothing -> ""
  myCards <- liftIO $ getCardsByDesigner name
  withAuth (\username -> renderUserPage username (fmap title myCards)) "user"

listKeywordsRoute :: Route
listKeywordsRoute = do
  keywords <- liftIO getKeywords
  lucidToSpock (renderKeywordPage keywords)

getFile :: String -> Route
getFile name = file (pack name) ("./files/" ++ name)

lucidToSpock :: Html () -> Route
lucidToSpock t = (html . toStrict . renderText) t

defaultPort :: String
defaultPort = "8080"

getPort :: IO String
getPort = do
  maybePort <- lookupEnv "PORT"
  port <- case maybePort of
    (Just port) -> return port
    Nothing -> do
      putStrLn $ "No PORT string found in environment, using default (" ++ defaultPort ++ ")."
      return defaultPort
  return port

withAuth :: (Text -> Html ()) -> Text -> ActionT IO ()
withAuth authenticatedRoute goHereAfterLogin = do
  maybeUsername <- cookie "username"
  case maybeUsername of
    Just username -> lucidToSpock (authenticatedRoute username)
    Nothing -> lucidToSpock $ renderMustLogIn "Please log in first." goHereAfterLogin
