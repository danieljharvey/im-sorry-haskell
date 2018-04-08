newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f x = Identity f (x)

instance Applicative Identity where
    pure x = Identity x
    Identity f <*> Identity a = Identity (f a)

let testF = Identity +1

let testA = Identity 10

main :: IO ()
main = testF <*> testA

