data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)

ans = [Thursday .. Sunday] 

main = putStrLn $ show ans

