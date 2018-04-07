import Control.Applicative
import Data.List (elemIndex)


-- first exercise
x :: Maybe Int
x = elemIndex 3 [1,2,3,4,5]

y :: Maybe Int
y = elemIndex 4 [1,2,3,4,5,6]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = pure max' <*> x <*> y


-- second exercise
sum' :: (Int, Int) -> Int
sum' (a,b) = a + b

xs = [1,2,3]
ys = [4,5,6]

x' :: Maybe Int
x' = lookup 3 $ zip xs ys

y' :: Maybe Int
y' = lookup 2 $ zip xs ys

summed :: Maybe Int
--summed = pure sum' <*> answer where
--    answer = pure (,) <*> x' <*> y'

summed = pure sum' <*> (pure (,) <*> x' <*> y')

main :: IO ()
main = print summed

