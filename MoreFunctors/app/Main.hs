module Main where

import Lib

data Sum b a =
    First a
  | Second b deriving (Show)

instance Functor (Sum e) where
    fmap f (First a) = First (f a)
    fmap f (Second b) = Second b

addExclaim x = x ++ "!!"

data More b a = 
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

main :: IO ()
main = putStrLn "Hello"
