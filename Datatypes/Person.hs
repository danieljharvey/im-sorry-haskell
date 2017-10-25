data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

gary = Person "Gary" "Horse" 67 126.7 "121212-1212" "eggs"



data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)

niceCar = Car {company="Ford", model="Mustang", year=1967}

loveString :: String -> String -> String
loveString name thing = name ++ " loves " ++ thing

garyStats = loveString (firstName gary) (flavor gary)

main = putStrLn $ show garyStats
