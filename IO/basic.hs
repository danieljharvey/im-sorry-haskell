import Data.Char

bigName name = map toUpper name

main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn "And what is a thing you like?"
    thing <- getLine
    putStrLn $ (bigName thing  ++ bigName name)
