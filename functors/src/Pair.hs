module Pair
    ( test
    , Pair
    ) where

import Test.QuickCheck
import Helpers

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

instance (Arbitrary a, Num a) => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Pair a b)

test :: IO ()
test = do
    putStrLn "PAIR FUNCTOR"
    putStrLn "id check"
    quickCheck $ \x -> functorIdentity (x :: Pair Int)
    putStrLn "compose check"
    quickCheck $ \y -> functorCompose (+50) (*1000) (y :: Pair Int)
