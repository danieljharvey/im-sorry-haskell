import Data.List (group)

pack :: Eq a => [a] -> [[a]]
pack [] = []
-- pack x = group x
pack (x:xs) = (x:(filter (==x) xs)):(pack $ filter (/=x) xs) 

answer = pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']

main = putStrLn $ show answer
