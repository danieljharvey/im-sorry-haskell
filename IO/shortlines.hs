main = do
    contents <- getContents
    putStr (shortLinesOnly contents)

lineIsShort line = length line < 21

shortLinesOnly :: String -> String
shortLinesOnly input = 
    let allLines = lines input
        shortLines = filter lineIsShort allLines
        result = unlines shortLines
    in  result