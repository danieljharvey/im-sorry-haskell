{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Level where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B

type Board = [[Int]]

type LevelIDStr = String;

data Level = Level { levelID :: Int, board :: Board } deriving (Show, Generic, ToJSON, FromJSON)

-- getEncodedJSON :: LevelIDStr -> B.ByteString
getEncodedJSON a = case getLevelJSON a of
  Just a -> encode a
  _ -> B.empty

getLevelJSON :: LevelIDStr -> Maybe Level
getLevelJSON a = showLevel <$> readMaybe a

showLevel :: Int -> Level
showLevel a = Level { levelID = a, board = [] }

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing



