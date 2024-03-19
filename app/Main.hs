module Main where

import Hangman(Gallow (Gallow), createGallow,gallowWithNewdotted)
import System.Random (randomRIO)

wordsList :: [String]
wordsList =
  [ "apple"
  , "banana"
  , "cherry"
  , "date"
  , "elderberry"
  , "fig"
  , "grape"
  , "honeydew"
  , "kiwi"
  , "lemon"
  , "mango"
  , "nectarine"
  , "orange"
  , "pear"
  , "quince"
  , "raspberry"
  , "strawberry"
  , "tangerine"
  , "watermelon"
  ]

loopGame :: Gallow -> Int -> IO ()
loopGame (Gallow _ _ True) _          = putStrLn "You win!"
loopGame _ 1                          = putStrLn "You lose!"
loopGame (Gallow word dotted win) try = do
  putStrLn dotted
  putStrLn ("You have " ++ show (try - 1) ++ " chances left.")
  putStrLn "Enter a letter: "
  x <- getLine
  let newGallow = gallowWithNewdotted (Gallow word dotted win) (head x)
  loopGame newGallow (try - 1)

main :: IO ()
main = do
  putStrLn "Welcome to Hangman!"
  putStrLn "Guess the word by entering a letter at a time."
  num <- randomRIO (0, length wordsList - 1) :: IO Int
  let gallow = createGallow (wordsList !! num)
  loopGame gallow 3
