module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = print poop

data StateFace a = Nothings | Everythings a

instance (Show a) => Show (StateFace a) where
    show Nothings = show "Nothing"
    show (Everythings a) = show a

-- initial = Nothings
other = Everythings 1

stateMap :: (Num a) => (a -> a) -> StateFace a -> StateFace a
stateMap _ Nothings = Nothings
stateMap f (Everythings a) = Everythings (f a)

-- sorted = stateMap (+1) initial
poop = stateMap (+1) other
