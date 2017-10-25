data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int 
                     } deriving (Eq, Show, Read)

mikeD = Person { firstName = "Michael", lastName = "Diamond", age = 43 }
adRock = Person { firstName = "Adam", lastName = "Horocitz", age = 41 }
mca = Person { firstName = "Adam", lastName = "Yauch", age = 44 }
 
beastieBoys = [mca, adRock, mikeD]

newMember = read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person

isMember :: [Person] -> Person -> Bool
isMember group member = member `elem` group

printMember :: [Person] -> Person -> String
printMember group member = if isMember group member then "Yes, " ++ (firstName member) ++ " is a member of the group" else "Nope, sorry"

main = putStrLn $ show (printMember beastieBoys newMember)
