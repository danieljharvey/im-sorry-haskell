module Identity
    ( test
    , Identity
    ) where

import Test.QuickCheck
import Helpers

data Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance (Arbitrary a, Num a) => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return (Identity a)

test :: IO ()
test = do
    putStrLn "IDENTITY FUNCTOR"
    putStrLn "id check"
    quickCheck $ \x -> functorIdentity (x :: Identity Int)
    putStrLn "compose check"
    quickCheck $ \y -> functorCompose (*10) (+100) (y :: Identity Int)
