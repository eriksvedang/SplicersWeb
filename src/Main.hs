{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Spock.Safe
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text.Internal (Text)
import Data.Monoid ((<>))
import Database (temp)

main :: IO ()
main = 
    runSpock 8080 $ spockT id $
    do get root $ text "Hej!"
       get ("hello" <//> var) hello
       get "db" $ do
         liftIO temp
         (text "performed db stuff")
         
hello :: MonadIO m => Text -> ActionT m a
hello name = html ("Hejsan <a href=\"#\">" <> name <> "</a>.")

