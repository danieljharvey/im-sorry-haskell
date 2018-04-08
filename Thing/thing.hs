data State a = Nope | Loading | Waiting | Data a | Failed deriving (Show)

initialState = Data [69]

secondState = Data [1,2,3,4,5,67]

instance Functor State where
    fmap f a = case a of 
                 Data b  -> Data (f b)
                 Loading -> Loading
                 Waiting -> Waiting
                 Failed  -> Failed
                 Nope    -> Nope

instance Monoid a => Monoid (State a) where
    mempty                  = Nope
    Nope `mappend` _        = Nope
    _ `mappend` Nope        = Nope
    Loading `mappend` _     = Loading
    _ `mappend` Loading     = Loading
    Waiting `mappend` _     = Waiting
    _ `mappend` Waiting     = Waiting
    Failed `mappend` _      = Failed
    _ `mappend` Failed      = Failed
    Data a `mappend` Data b = Data $ a `mappend` b

instance Applicative State where
    pure  = Data
    Data f <*> Data a = Data (f a)
    _ <*> _ = Nope

func = Data (\x -> x `mappend` [12])

solved = func <*> secondState

sorted = initialState `mappend` secondState

main = print $ solved
                  
