data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

-- answer = flatten (Elem 5)
answer = flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])

main = putStrLn $ show answer
