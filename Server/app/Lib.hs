{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( runServer
    ) where

import Web.Scotty
import Data.Monoid (mconcat)
import Level
import qualified Data.ByteString.Lazy as B

runServer :: IO ()
runServer = scotty 3000 $ do
    get "/" $
        html "<h1>Bum</h1>"

    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
    
    get "/level/:levelID" $ do
        levelIDString <- param "levelID"
        bs <- getEncodedJSON levelIDString
        html "Poo"
