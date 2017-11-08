things = ["turd", "bird", "third"]

main = do
    mapM print things
    rs <- sequence [getLine, getLine, getLine]    
    print rs 
