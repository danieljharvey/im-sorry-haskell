module Three2
    ( test
    , Three'
    ) where

import Test.QuickCheck

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

threeGen :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)

threeGen = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three' a b c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = threeGen

threeGenMix :: Gen (Three' Int String)
threeGenMix = threeGen

testAssoc :: Three' Int String -> Bool
testAssoc a = fmap ((++ "horse") . (++ " time")) a == (fmap (++ "horse") . fmap (++ " time")) a

propTestAssoc :: Property
propTestAssoc = forAll threeGenMix testAssoc

test :: IO ()
test = quickCheck propTestAssoc
