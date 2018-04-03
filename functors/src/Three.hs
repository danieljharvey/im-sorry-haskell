module Three
    ( test
    , Three
    ) where

import Test.QuickCheck

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)

threeGen = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = threeGen

threeGenMix :: Gen (Three Int Int String)
threeGenMix = threeGen

testAssoc :: Three Int Int String -> Bool
testAssoc a = fmap ((++ "horse") . (++ " time")) a == (fmap (++ "horse") . fmap (++ " time")) a

propTestAssoc :: Property
propTestAssoc = forAll threeGenMix testAssoc

test :: IO ()
test = quickCheck propTestAssoc
