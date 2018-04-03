module Main where

import qualified Identity
import qualified Pair
import qualified Two
import qualified Three
import qualified Three2
import qualified Four
import qualified Four2

main :: IO ()
main = do
    Identity.test
    Pair.test
    Two.test
    Three.test
    Three2.test
    Four.test
    Four2.test
