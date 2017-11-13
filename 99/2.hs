myButLast :: [a] -> a
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs

answer = myButLast ['a'..'z'] 

main = putStrLn $ "Answer should be 'y' but its " ++ show answer
