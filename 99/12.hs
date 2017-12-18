import Data.List

data ListItem a = Single a | Multiple Int a deriving (Show)

encode :: Eq a => [a] -> [(Int, a)]
encode xs = [(length x, head x) | x <- group xs]

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
    where
        encodeHelper (1,x) = Single x
        encodeHelper (n,x) = Multiple n x

firstAnswer = encodeModified ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']


decodeModified :: Eq a => [ListItem a] -> [a]
decodeModified [] = []
decodeModified xs = concat (map decodeHelper xs)
    where
        decodeHelper (Single x) = [x]
        decodeHelper (Multiple n x) = replicate n x

answer = decodeModified firstAnswer

main = putStrLn $ show answer
