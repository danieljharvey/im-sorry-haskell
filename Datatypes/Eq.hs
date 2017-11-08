import Prelude hiding (Eq)
import Data.Maybe

class Eq a where
    (.==) :: a -> a -> Bool
    (./=) :: a -> a -> Bool
    x .== y = not (x ./= y)
    x ./= y = not (x .== y)

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red .== Red = True
    Green .== Green = True
    Yellow .== Yellow = True
    _ .== _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

lights = [Red, Yellow, Green]

instance (Eq m) => Eq (Maybe m) where
    Just x .== Just y = x .== y
    Nothing .== Nothing = True
    _ .== _ = False

getMaybe :: Int -> Maybe Int
getMaybe a = Just a

answer = getMaybe 6 -- Comment

main = putStrLn $ show answer

