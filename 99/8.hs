compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:ys@(y:_))
    | x == y = compress ys
    | otherwise = x : compress ys
compress ys = ys

answer = compress "aaabbbcccddd"

main = putStrLn $ show answer
