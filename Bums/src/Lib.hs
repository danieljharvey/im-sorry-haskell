module Lib
    ( someFunc
    ) where

import Control.Monad
import Data.Either
import Data.Char (toLower)
import Data.List (isInfixOf)

data DogErrors = NameTooShort | NameTooLong | ContainsS | IsNotBrown | UnknownColour deriving (Eq)

type DogName = String
data DogColour = Brownish | Yellowish deriving (Eq, Show)

data Dog = Doge DogName DogColour deriving (Eq)
instance Show Dog where
    show (Doge name colour) = "A good boy called " ++ name ++ " with hair of the colour " ++ show colour

instance Show DogErrors where
    show NameTooShort  = "Name is too short"
    show NameTooLong   = "Name is way too long"
    show ContainsS     = "Name contains the letter s"
    show IsNotBrown    = "Dog is not brown"
    show UnknownColour = "Don't know what that colour is"

strLower name = fmap toLower name

-- Name validation

nameNotEmpty :: DogName -> Either [DogErrors] DogName
nameNotEmpty "" = Left [NameTooShort]
nameNotEmpty s = Right s

nameNotTooLong :: DogName -> Either [DogErrors] DogName
nameNotTooLong s
  | (length s) > 10 = Left [NameTooLong]
  | otherwise       = Right s

containsS :: DogName -> Either [DogErrors] DogName
containsS name
  | 's' `elem` (strLower name) = Left [ContainsS]
  | otherwise                 = Right name

nameChecks :: DogName -> Either [DogErrors] DogName
nameChecks = nameNotEmpty >=> nameNotTooLong >=> containsS

-- Colour validation

stringToColour :: String -> Either [DogErrors] DogColour
stringToColour s
  | (strLower s) `isInfixOf` "brown"  = Right Brownish
  | (strLower s) `isInfixOf` "yellow" = Right Yellowish
  | otherwise              = Left [UnknownColour]

colourIsBrown :: DogColour -> Either [DogErrors] DogColour
colourIsBrown Brownish = Right Brownish
colourIsBrown _        = Left [IsNotBrown] 

colourChecks :: String -> Either [DogErrors] DogColour
colourChecks = stringToColour >=> colourIsBrown

getDog :: DogName -> String -> Either [DogErrors] Dog
getDog name colourString = do
    dogName <- nameChecks name
    colour <- colourChecks colourString
    return $ Doge dogName colour

displayDog :: Either [DogErrors] Dog -> String
displayDog (Right dog)   = show dog
displayDog (Left errors) = "No dog of that name: " ++ (concatMap show errors)

someFunc :: IO ()
someFunc = do
    putStrLn "What is the name of this dog?"
    name <- getLine
    putStrLn "And what colour is this dog?"
    colour <- getLine
    putStrLn "......"
    putStrLn $ displayDog $ getDog name colour
