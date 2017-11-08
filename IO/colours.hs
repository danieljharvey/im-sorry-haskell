import Control.Monad

stuff = ["horse", "yes", "desk"]

questionTime a = do
    putStrLn $ "What do you like about " ++ show a ++ "?"
    getLine

main = do
    colors <- mapM questionTime stuff
    putStrLn "The things you like are: "
    mapM putStrLn colors
