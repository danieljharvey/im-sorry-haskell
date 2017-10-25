import qualified Data.Map as Map
import Data.List

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
      Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
      Just (state, code) -> if state /= Taken
                            then Right code
                            else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList[
    (100,(Taken, "ZD39I")),
    (101,(Free, "JAH01")),
    (103,(Taken, "Horse")),
    (104,(Free, "Hat"))]

getOne = lockerLookup 100 lockers

getTwo = lockerLookup 101 lockers

getThree = lockerLookup 102 lockers

getFour = lockerLookup 103 lockers

getFive = lockerLookup 104 lockers

retrieveCode :: Either String Code -> String
retrieveCode maybe = case maybe of
                       Left (code) -> code
                       Right (message) -> message

answers = [getOne, getTwo, getThree, getFour, getFive]

retrieved = map retrieveCode answers

concatted = intercalate " - " retrieved

main = putStrLn $ show concatted 
