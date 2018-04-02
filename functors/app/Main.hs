module Main where

import qualified Identity
import qualified Pair
import qualified Two

main :: IO ()
main = do
    Identity.test
    Pair.test
    Two.test
