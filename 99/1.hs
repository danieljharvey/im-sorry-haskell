myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

-- answer = myLast [1,2,3,4]
answer = myLast ['x', 'y', 'z']

main = putStrLn $ "Should be z but it's " ++ show answer
