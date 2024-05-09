{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Gallow
  ( Gallow (..),  
    game,
    addNewWord,
    showWords    
  )
where
import System.Random (randomRIO)
import System.Console.ANSI (clearScreen, setCursorPosition)
import Paths_Hangman (getDataFileName)

type Dotted = [Char]

data Gallow = Gallow
  { word :: String,
    dotted :: Dotted,
    completed :: Bool
  }
  deriving (Show)

wordsTxt :: IO FilePath
wordsTxt = getDataFileName "words.txt"

clear :: IO ()
clear = clearScreen >> setCursorPosition 0 0

starter :: IO ()
starter = putStrLn "Welcome to Hangman!" >> putStrLn "Guess the word by entering a letter at a time."

getWords :: IO [String]
getWords = do
  content <- readFile =<< wordsTxt
  return $ lines content

numRandom :: [a] -> IO Int
numRandom xs = randomRIO (0, length xs - 1)

createGallow :: String -> Gallow
createGallow word = Gallow word (replicate (length word) '_') False

printDotted :: Gallow -> IO ()
printDotted (Gallow _ dotted _) = putStrLn dotted

revelLetter :: Dotted -> String -> Char -> Dotted
revelLetter dotted word letter = [if x == letter then x else y | (x, y) <- zip word dotted]

gallowWithNewdotted :: Gallow -> Char -> Gallow
gallowWithNewdotted (Gallow word dotted _) letter = 
  Gallow word (revelLetter dotted word letter) (word == revelLetter dotted word letter)

numberOfTry :: Gallow -> Int
numberOfTry (Gallow word _ _) = length word * 5

game :: IO ()
game = do
  starter
  words <- getWords
  randomIndex <- numRandom words
  let word = words !! randomIndex
  let gallow = createGallow word
  loopGame gallow (numberOfTry gallow)

isExit :: String -> Bool
isExit "menu" = True
isExit _      = False

revealDotter :: Gallow -> String -> Gallow
revealDotter x [] =  x
revealDotter gallow (x:xs) = 
  revealDotter (gallowWithNewdotted gallow x) xs

loopGame :: Gallow -> Int -> IO ()
loopGame (Gallow _ _ True) _          = putStrLn "You win!"
loopGame _ 1                          = putStrLn "You lose!"
loopGame gallow try = do
  printDotted gallow
  putStrLn ("You have " ++ show (try - 1) ++ " chances left.")
  putStrLn "Enter a letter: "
  x <- getLine  
  if isExit x
    then clear
    else let newGallow = revealDotter gallow x
         in loopGame newGallow (try - 1)

showWords :: IO () 
showWords = do
  clear 
  words <- getWords
  mapM_ putStrLn words
  putStrLn "Press enter to return to the menu."
  _ <- getLine
  clear

addNewWord :: IO ()
addNewWord = do
  clear
  putStrLn "Enter a new word: "
  newWord <- getLine
  words <- getWords
  (if isAlreadyExistsWord newWord words
    then putStrLn "The word already exists."       
    else do
      filepath <- wordsTxt
      appendFile filepath ('\n' : newWord)
      putStrLn "The word has been added.")
  putStrLn "Press enter to return to the menu."
  _ <- getLine
  clear

isAlreadyExistsWord :: String -> [String] -> Bool
isAlreadyExistsWord newWord words = newWord `elem` words