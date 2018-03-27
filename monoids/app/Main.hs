module Main where

import Data.Monoid

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance (Monoid a) => Monoid (Optional a) where
    mempty = Nada
    mappend Nada b = b
    mappend a Nada = a
    mappend (Only a) (Only b) = Only (mappend a b)

firstFigure = Product 32

secondFigure = Product 64

combined = mappend (Only firstFigure) (Only secondFigure)

failed = mappend (Only firstFigure) (Nada)

failed2 = mappend (Nada) (Only secondFigure)

main :: IO ()
main = putStrLn "Horses"
