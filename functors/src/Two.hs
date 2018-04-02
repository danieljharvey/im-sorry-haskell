module Two
    ( test
    , Two
    ) where

import Test.QuickCheck

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

identityGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)

identityGen = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = identityGen

twoGenInt :: Gen (Two Int Int)
twoGenInt = identityGen

testAssoc :: Two Int Int -> Bool
testAssoc a = fmap ((+1) . (*2)) a == (fmap (+1) . fmap (*2)) a

propTestAssoc :: Property
propTestAssoc = forAll twoGenInt testAssoc

test :: IO ()
test = quickCheck propTestAssoc
