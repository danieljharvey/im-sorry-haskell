module Four2
    ( test
    , Four'
    ) where

import Test.QuickCheck

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

fourGen :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)

fourGen = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four' a b c d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = fourGen

fourGenMix :: Gen (Four' Int String)
fourGenMix = fourGen

testAssoc :: Four' Int String -> Bool
testAssoc a = fmap ((++ "horse") . (++ " time")) a == (fmap (++ "horse") . fmap (++ " time")) a

propTestAssoc :: Property
propTestAssoc = forAll fourGenMix testAssoc

test :: IO ()
test = quickCheck propTestAssoc
