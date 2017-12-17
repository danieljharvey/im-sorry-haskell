import Data.List 

encode :: Eq a => [a] -> [(Int, a)]
-- encode = map (\x -> (length x, head x)) . group
encode xs = [(length x, head x) | x <- group xs]

answer = encode ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']

main = putStrLn $ show answer
