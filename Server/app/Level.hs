{-# LANGUAGE DeriveGeneric #-}

module Level where

import Data.Aeson (ToJSON)
import GHC.Generics

type Board = [[Int]]

data Level = Level { levelID :: Int, board :: Board } deriving (Show, Generic)

instance ToJSON Level

getLevelJSON :: String -> String
getLevelJSON a = case fmap showLevel levelID of 
                   Just a -> show a
                   _ -> "Nothing found"
                 where levelID = readMaybe a

showLevel :: Int -> Level
showLevel a = Level { levelID = a, board = [] }

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing



