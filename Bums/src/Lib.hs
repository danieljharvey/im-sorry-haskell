module Lib
    ( someFunc
    ) where

import Control.Monad
import Data.Either
import Data.Char (toLower)
import Data.List (isInfixOf)
import Valid (nameValid, colourValid)
import Dog

strLower :: String -> String
strLower = fmap toLower

getDog :: DogName -> String -> Either [DogErrors] Dog
getDog name colourString = do
    dogName <- nameValid name
    colour <- colourValid colourString
    return $ Doge dogName colour

displayDog :: Either [DogErrors] Dog -> String
displayDog (Right dog)   = show dog
displayDog (Left errors) = "No dog of that name: " ++ concatMap ((++) ", " . show) errors

-- Business time

someFunc :: IO ()
someFunc = do
    putStrLn "What is the name of this dog?"
    name <- getLine
    putStrLn "And what colour is this dog?"
    colour <- getLine
    putStrLn "......"
    putStrLn $ displayDog $ getDog name colour
