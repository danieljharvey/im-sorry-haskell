module Four
    ( test
    , Four
    ) where

import Test.QuickCheck
import Helpers

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return (Four a b c d)

test :: IO ()
test = do
    putStrLn "FOUR FUNCTOR"
    putStrLn "id check:"
    quickCheck $ \x -> functorIdentity (x :: Four Int String String Int)
    putStrLn "compose check:"
    quickCheck $ \y -> functorCompose (*10) (+500) (y :: Four Int String String Int)

