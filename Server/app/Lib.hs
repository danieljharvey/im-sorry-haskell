{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( runServer
    ) where

import Web.Scotty
import Data.Monoid (mconcat)
import Data.Text.Lazy
import Level

runServer :: IO ()
runServer = scotty 3000 $ do
    get "/" $
        html "<h1>Bum</h1>"

    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
    
    get "/level/:levelID" $ do
        levelIDString <- param "levelID"
        json $ getLevelJSON levelIDString
