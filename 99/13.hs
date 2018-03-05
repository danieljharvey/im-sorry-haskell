import Data.List

data Thing a = Single a | Multiple Int a deriving (Show)

encodeDirect :: List char -> List Thing
encodeDirect [] = []

answer = encodeDirect "aaaabccaadeeee"

main = show answer
