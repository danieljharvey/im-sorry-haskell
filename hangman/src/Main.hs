module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.Console.ANSI

type WordList = [String]

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 7

filterWords :: String -> Bool
filterWords w = (length w > minWordLength) && (length w < maxWordLength)

gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter filterWords aw)

randomWord :: WordList -> IO String
randomWord wl = do
    randomIndex <- randomRIO(0, length wl -1)
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] String

instance Show Puzzle where
    show (Puzzle word discovered guessed) = 
        (intersperse ' ' $ fmap renderPuzzleChar discovered) ++ "\n\nGuessed so far: " ++ filterIncorrect word guessed

makeNothing :: a -> Maybe a
makeNothing _ = Nothing

freshPuzzle :: String -> Puzzle
freshPuzzle w = Puzzle w (fmap makeNothing w) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = elem c s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = elem c guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c
renderPuzzleChar _ = '_'

zipper :: (Eq a) => a -> a -> Maybe a -> Maybe a
zipper guessed wordChar guessChar =
    if wordChar == guessed
       then Just wordChar
       else guessChar

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
    Puzzle word newFilledInSoFar (c : s)
    where newFilledInSoFar = zipWith (zipper c) word filledInSoFar

colorText :: String -> Color -> IO ()
colorText s c = do
    setSGR [SetColor Foreground Vivid c]
    putStrLn s
    setSGR [Reset]

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of 
      (_, True) -> do
          colorText "You already guessed that character, pick something else" Red
          return puzzle
      (True, _) -> do
          colorText "This character was in the word, filling in accordingly" Green
          return (fillInCharacter puzzle guess)
      (False, _) -> do
          colorText "This character wasn't in the word, try again" Red
          return (fillInCharacter puzzle guess)

filterIncorrect :: Eq a => [a] -> [a] -> [a]
filterIncorrect word guessed = filter (\x -> not (elem x word)) guessed

countIncorrect :: Puzzle -> Int
countIncorrect (Puzzle word _ guessed) = length $ filterIncorrect word guessed

gameOver :: Puzzle -> IO ()
gameOver puzzle@(Puzzle wordToGuess _ guessed) =
    if countIncorrect puzzle > 7 then
                          do colorText "You lose!" Red
                             putStrLn $ "The word was: " ++ wordToGuess
                             exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) = 
    if all isJust filledInSoFar then
                                do colorText "You win!" Green
                                   exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    colorText ("Current puzzle is: " ++ show puzzle ++ "\n") Yellow
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle c >>= runGame
      _   -> colorText "Your guess must be a single character" Red

main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle
