module Main where

import qualified Identity
import qualified Pair

main :: IO ()
main = do
    Identity.test
    Pair.test
