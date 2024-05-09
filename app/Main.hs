module Main where

import Gallow
    ( game,
      addNewWord,
      showWords)
import System.Console.ANSI (setCursorPosition, clearScreen)
import Prelude hiding (words)

main :: IO ()
main = menu

menu :: IO ()
menu = do
  putStrLn "1. Play Hangman"
  putStrLn "2. Show the list of words"
  putStrLn "3. Add new word"
  putStrLn "4. Exit"
  putStrLn "Enter your choice: "
  choice <- getLine
  case choice of
    "1" -> game >> menu
    "2" -> showWords >> menu
    "3" -> addNewWord >> menu
    "4" -> putStrLn "Goodbye!" >> clearScreen >> setCursorPosition 0 0    
    _ -> putStrLn "Invalid choice" >> menu