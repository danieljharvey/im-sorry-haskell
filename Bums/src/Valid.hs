{-# LANGUAGE DeriveFunctor #-}

module Valid
    ( nameValid
    , colourValid
    ) where

import Data.Char (toLower)
import Data.List (isInfixOf)
import Dog

newtype Validation e a = Validation { getValidation :: Either e a }
  deriving Functor

instance Monoid e => Applicative (Validation e) where
  pure = Validation . Right
  Validation a <*> Validation b = Validation $
    case a of
      Right va -> fmap va b
      Left ea -> either (Left . mappend ea) (const $ Left ea) b

strLower :: String -> String
strLower = fmap toLower      

-- NAME VALIDATION

nameValid :: DogName -> Either [DogErrors] DogName
nameValid name = getValidation $
    Validation (nameNotEmpty name) *> 
    Validation (nameNotTooLong name) *> 
    Validation (containsS name) *>
    Validation (isSteve name)

nameNotEmpty :: DogName -> Either [DogErrors] DogName
nameNotEmpty "" = Left [NameTooShort]
nameNotEmpty s  = Right s

nameNotTooLong :: DogName -> Either [DogErrors] DogName
nameNotTooLong s
  | length s > 10   = Left [NameTooLong]
  | otherwise       = Right s

containsS :: DogName -> Either [DogErrors] DogName
containsS name
  | 's' `elem` strLower name  = Left [ContainsS]
  | otherwise                 = Right name
  
isSteve :: DogName -> Either [DogErrors] DogName
isSteve name
  | "steve" `isInfixOf` strLower name = Left [ForFucksSakeSteve]
  | otherwise                          = Right name




-- COLOUR VALIDATION

colourValid :: String -> Either [DogErrors] DogColour
colourValid string = getValidation $
    Validation (colourIsUnknown color) *>
    Validation (colourIsBrown color)
    where color = stringToColour string

stringToColour :: String -> DogColour
stringToColour s
  | strLower s `isInfixOf` "brown"  = Brownish
  | strLower s `isInfixOf` "yellow" = Yellowish
  | strLower s `isInfixOf` "grey" = Grey
  | strLower s `isInfixOf` "gray" = Grey
  | strLower s `isInfixOf` "black" = Black
  | strLower s `isInfixOf` "dark" = Black
  | strLower s `isInfixOf` "white" = White
  | strLower s `isInfixOf` "light" = White  
  | otherwise              = Dunno

colourIsUnknown :: DogColour -> Either [DogErrors] DogColour
colourIsUnknown Dunno = Left [UnknownColour]
colourIsUnknown c = Right c

colourIsBrown :: DogColour -> Either [DogErrors] DogColour
colourIsBrown Brownish = Left [IsBrown]
colourIsBrown c        = Right c
