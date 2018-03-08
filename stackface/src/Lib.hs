module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn (show poop ++ " " ++ show scoop ++ " equals " ++ show hoop ++ " but also " ++ show noop)

data StateFace a = Nothings | Everythings a

instance (Show a) => Show (StateFace a) where
    show Nothings = "Nothing"
    show (Everythings a) = "Somethings " ++ show a

instance Functor StateFace where
    fmap _ Nothings = Nothings
    fmap f (Everythings a) = Everythings (f a)

instance Applicative StateFace where
    pure = Everythings
    (Everythings f) <*> (Everythings x) = Everythings(f x)
    _ <*> _ = Nothings

-- initial = Nothings
other = Everythings 100

-- create two StateFace functors
poop = fmap (*100) other
scoop = fmap (+1000) poop

-- use ap to do addition inside them
hoop = fmap (+) poop <*> scoop

-- still the rules apply though - Nothings ruins it all
noop = fmap (+) hoop <*> Nothings