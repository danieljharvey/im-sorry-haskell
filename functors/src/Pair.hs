module Pair
    ( test
    , Pair
    ) where

import Test.QuickCheck

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

identityGen :: Arbitrary a => Gen (Pair a)

identityGen = do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = identityGen

pairGenInt :: Gen (Pair Int)
pairGenInt = identityGen

testAssoc :: Pair Int -> Bool
testAssoc a = fmap ((+1) . (*2)) a == (fmap (+1) . fmap (*2)) a

propTestAssoc :: Property
propTestAssoc = forAll pairGenInt testAssoc

test :: IO ()
test = quickCheck propTestAssoc
