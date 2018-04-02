module MainSpec where

import Test.Hspec
import Test.QuickCheck
import Main

main :: IO ()
main = hspec $ do
  describe "Hangman" $ do
    it "Filters successful guesses from list" $ do
        filterIncorrect "poo" "ob" `shouldBe` "b"

--    it "returns the first element of an *arbitrary* list" $
--      property $ \x xs -> head (x:xs) == (x :: Int)


