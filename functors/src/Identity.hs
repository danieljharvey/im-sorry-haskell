module Identity
    ( test
    , Identity
    ) where

import Test.QuickCheck

data Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

identityGen :: Arbitrary a => Gen (Identity a)

identityGen = do
    a <- arbitrary
    return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

testAssoc :: Identity Int -> Bool
testAssoc a = fmap ((+1) . (*2)) a == (fmap (+1) . fmap (*2)) a

propTestAssoc :: Property
propTestAssoc = forAll identityGenInt testAssoc

test :: IO ()
test = quickCheck propTestAssoc
