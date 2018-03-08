module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn (show poop ++ " + " ++ show scoop ++ " equals " ++ show hoop ++ " but it could have been " ++ show noop)

data StateFace a = Nothings | Everythings a

-- Make this data type showable
instance (Show a) => Show (StateFace a) where
    show Nothings = "Nothing"
    show (Everythings a) = show a

-- Make this data type mappable
instance Functor StateFace where
    fmap _ Nothings = Nothings
    fmap f (Everythings a) = Everythings (f a)

-- Make this data type applicative-able
instance Applicative StateFace where
    pure = Everythings
    (Everythings f) <*> (Everythings x) = Everythings(f x)
    _ <*> _ = Nothings

-- Create base value
other :: StateFace Integer
other = Everythings 100

-- create two StateFace functors
poop :: StateFace Integer
poop = fmap (*100) other

scoop :: StateFace Integer
scoop = fmap (+1000) poop

-- use ap to do addition inside them
hoop :: StateFace Integer
hoop = fmap (+) poop <*> scoop

-- still the rules apply though - Nothings ruins it all
noop :: StateFace Integer
noop = fmap (+) hoop <*> Nothings