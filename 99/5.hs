myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

answer = myReverse "yevrahjleinad"

main = putStrLn $ show answer
