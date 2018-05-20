module Dog where

data DogErrors = NameTooShort | NameTooLong | ContainsS | IsBrown | UnknownColour | ForFucksSakeSteve deriving (Eq)

type DogName = String
data DogColour = Dunno | Brownish | Yellowish | Black | Grey | White deriving (Eq, Show)

data Dog = Doge DogName DogColour deriving (Eq)
instance Show Dog where
    show (Doge name colour) = "A good boy called " ++ name ++ " with hair of the colour " ++ show colour

instance Show DogErrors where
    show NameTooShort  = "Name is too short"
    show NameTooLong   = "Name is way too long"
    show ContainsS     = "Name contains the letter s"
    show IsBrown    = "Dog is brown which is boring"
    show UnknownColour = "Don't know what that colour is"
    show ForFucksSakeSteve = "For fucks sake, Steve, quit it"
    show _ = "Other problem, there's always problems"