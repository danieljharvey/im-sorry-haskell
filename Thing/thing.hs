data State a = Loading | Refreshing a | Data a | Failed deriving (Show)

initialState = Loading

secondState = Data [1,2,3,4,5,67]

stateMap :: (State a -> State b) -> State a -> State b
stateMap f Refreshing a = case state of 
                 Refreshing b -> Refreshing (f b)
                 Data b -> Data b
                 _ -> a

sorted = stateMap (+1) initialState

main = print $ sorted
                  
