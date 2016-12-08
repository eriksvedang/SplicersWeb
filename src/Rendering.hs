{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Rendering where

import Control.Monad (foldM, mapM_)
import Data.Text.Internal (Text)
import Data.Text (unpack, pack)
import Data.Monoid ((<>))
import Card
import Keyword
import Lucid

css :: Text -> Html ()
css name = link_ [rel_ "stylesheet", type_ "text/css", href_ name]

allCSS :: Html ()
allCSS = do (css "/files/styles.css")
            (css "/files/card.css")
            (css "http://fonts.googleapis.com/css?family=Karla:400,700,400italic,700italic")

renderPage :: Html () -> Html ()
renderPage body = do head_ $ do
                      (script_ [src_ "/files/jquery.min.js"] "")
                      (script_ [src_ "/files/scripts.js"] "")
                      (script_ [src_ "https://cdn.rawgit.com/showdownjs/showdown/1.5.1/dist/showdown.min.js"] "")

                      allCSS
                     body_ $ do
                       renderSmallMenu
                       body


renderFrontPage :: Html ()
renderFrontPage = renderPage $ do div_ [id_ "page"] $ do
                                    renderMenu
                                    div_ [] $ do
                                      article_ (toHtml "Hello")

renderCards :: [Card] -> Html ()
renderCards cards = renderPage $ do a_ [ href_ "add-card"] $ do
                                      div_ [class_ "add"] $ do
                                        span_ [] (toHtml "+ Create a card")
                                    mapM_ (renderCard AsLink) cards
                                    input_ [ type_ "text", name_ "filter", id_ "filter"]

data RenderCardMode = AsLink | NoLink

renderSingleCardPage :: Text -> [Card] -> Html ()
renderSingleCardPage title cards =
  renderPage $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
        h1_ [] (toHtml title)
        h2_ [] (toHtml $ "Designed by " <> (designer (head cards)))
        a_ [href_ "/cards"] "Cards"
      div_ [class_ "preview randomcolor"] $ do
        mapM_ (\card -> p_ [] (renderCard NoLink card)) cards

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


renderAddCard :: Text -> Html ()
renderAddCard username =
  renderPage $ do
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
              option_ [value_ "Air"] (toHtml "Air")
              option_ [value_ "Artificial"] (toHtml "Artificial")
              option_ [value_ "Bug"] (toHtml "Bug")
              option_ [value_ "Fungi"] (toHtml "Fungi")
              option_ [value_ "Mini"] (toHtml "Mini")
              option_ [value_ "Plant"] (toHtml "Plant")
              option_ [value_ "Sea"] (toHtml "Sea")
              option_ [value_ "Sin"] (toHtml "Sin")
              option_ [value_ "Terran"] (toHtml "Terran")
            select_ [name_ "gene1"] $ do
              option_ [value_ "Air"] (toHtml "Air")
              option_ [value_ "Artificial"] (toHtml "Artificial")
              option_ [value_ "Bug"] (toHtml "Bug")
              option_ [value_ "Fungi"] (toHtml "Fungi")
              option_ [value_ "Mini"] (toHtml "Mini")
              option_ [value_ "Plant"] (toHtml "Plant")
              option_ [value_ "Sea"] (toHtml "Sea")
              option_ [value_ "Sin"] (toHtml "Sin")
              option_ [value_ "Terran"] (toHtml "Terran")
          br_ []
          field "startMatter" """" "number" "4"
          field "startCards" "Cards" " (The number of cards you start with)" "number" "4"
          field "rules" "Rule text" " (example: @: Roam)" "text" ""
          field "flavor" "Flavour text" "" "text" ""
          input_ [type_ "text", name_ "designer", value_ username, readonly_ ""]
          field "illustration" "Image" " (URL)" "text" ""
          br_ []
          input_ [type_ "submit", value_ "Submit"]
      div_ [class_ "preview randomcolor"] $ do
        renderCard NoLink (Card "title" "rules" 0 Ting "type" NoGene NoGene 4 4 "" username "")


renderSubmittedCard :: Text -> Html ()
renderSubmittedCard title =
  renderPage $ do
    p_ $ toHtml $ "The card " <> title <> " was added!"
    a_ [href_ "/cards"] "Cards"

renderAddFakeData :: Html ()
renderAddFakeData = do
  renderPage $ p_ "Added fake data!"

renderPlayerPage :: Text -> [Text] -> Html ()
renderPlayerPage username myCardTitles = do
  renderPage $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
        h1_ (toHtml username)
        h2_ "Cards by me"
        mapM_ (\cardTitle -> li_ $ a_ [href_ $ pack ("/card/" ++ unpack cardTitle)] (toHtml cardTitle)) myCardTitles
        a_ [href_ "/"] "Front page"
        a_ [href_ "/logout"] "Log out"

renderSignupForm :: Html ()
renderSignupForm = do
  renderPage $ do
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
          input_ [type_ "submit", value_ "Sign up!"]

renderFailSignup :: Html ()
renderFailSignup = do
  renderPage $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
        h1_ "Failed to sign up"
        p_ "Perhaps that username is taken."

renderLoginFormFull :: Html ()
renderLoginFormFull = do
  renderPage $ do
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
    input_ [type_ "submit", value_ "Login"]

renderMustLogIn :: Text -> Text -> Html ()
renderMustLogIn helpText nextPage = do
  renderPage $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
        p_ (toHtml helpText)
        renderLoginForm nextPage

renderSucceededToLogin :: Html ()
renderSucceededToLogin = do
  renderPage $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
          p_ "You are logged in!"

renderFailedToLogin :: Html ()
renderFailedToLogin = do
  renderPage $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
          p_ "Failed to login, invalid password or username."

renderLogout :: Html ()
renderLogout = do
  renderPage $ do
    div_ [class_ "window"] $ do
      div_ [class_ "content"] $ do
          p_ "You have been logged out."

renderKeywordPage :: [Keyword] -> Html ()
renderKeywordPage keywords = do
  renderPage $ do
    mapM_ renderKeyword keywords

renderKeyword :: Keyword -> Html ()
renderKeyword kw = do h2_ (toHtml (keywordName kw))
                      p_ (toHtml (keywordRules kw))

renderRulesDocument :: Text -> Html ()
renderRulesDocument rulesText = do
  renderPage $ do
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


renderSmallMenu :: Html ()
renderSmallMenu = do
      div_ [id_ "smallmenu"] $ do
        renderMenuItems

renderMenuItems :: Html ()
renderMenuItems = do
  a_ [class_ "menu-link randomcolor", href_ "/"] $ toHtml "Start"
  a_ [class_ "menu-link randomcolor", href_ "/rules"] $ toHtml "Rules"
  a_ [class_ "menu-link randomcolor", href_ "/cards"] $ toHtml "Cards"
  a_ [class_ "menu-link randomcolor", href_ "/user"] $ toHtml "User"
