import System.IO

main = do
    withFile "words.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
