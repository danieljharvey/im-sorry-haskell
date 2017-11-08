import System.IO

main = do
    handle <- openFile "words.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
