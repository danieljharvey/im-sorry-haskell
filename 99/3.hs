elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) i = elementAt xs (i - 1)

answer = elementAt [1,2,4] 3 

main = putStrLn $ "I'm hoping for a 4 but its " ++ show answer
