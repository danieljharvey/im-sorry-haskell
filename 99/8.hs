import Data.List

compress :: (Eq a) => [a] -> [a]
compress = map head . group

answer = compress "aaaabccaadeeee"

main = putStrLn $ show answer
