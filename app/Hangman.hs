{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Hangman
  ( Gallow (..),
    createGallow,
    printDotted,
    gallowWithNewdotted,
  )
where

type Dotted = [Char]

data Gallow = Gallow
  { word :: String,
    dotted :: Dotted,
    completed :: Bool
  }
  deriving (Show)

createGallow :: String -> Gallow
createGallow word = Gallow word (replicate (length word) '_') False

printDotted :: Gallow -> IO ()
printDotted (Gallow _ dotted _) = putStrLn dotted

revelLetter :: Dotted -> String -> Char -> Dotted
revelLetter dotted word letter = [if x == letter then x else y | (x, y) <- zip word dotted]

gallowWithNewdotted :: Gallow -> Char -> Gallow
gallowWithNewdotted (Gallow word dotted _) letter = Gallow word (revelLetter dotted word letter) (word == revelLetter dotted word letter)