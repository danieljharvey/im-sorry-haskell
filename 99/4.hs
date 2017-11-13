myLength :: [a] -> Int
myLength [] = 0
myLength xs = sum (map (\x -> 1) xs)

yourLength :: [a] -> Int
yourLength [] = 0
yourLength (_:xs) = 1 + myLength xs

answer = yourLength [1,2,3,4,6,7,8]

main = putStrLn $ "Hoping for 7..." ++ show answer
