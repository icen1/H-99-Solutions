module Main where

import Lib
import System.Random
import ExamQuestions

main :: IO ()
main = putStr $ unlines (fizzBuzz)

guessingGame :: IO()
guessingGame = do
    putStrLn "Let's play a game!"
    putStrLn "I will pick a number between 1 and 100. You have to guess it."
    randNum <- randomRIO (1,100)
    number <- getContents
    let numberGiven = read number :: Int
    if numberGiven == randNum
        then do
            putStrLn "You guessed it!"
        else do
            if numberGiven > randNum
                then do
                    putStrLn "Your guess is too high"
                    main
                else do
                    putStrLn "Your guess is too low"
                    main

echo :: IO ()
echo = do
    lineGiven <- getLine
    let lineSplit = splitAt 5 lineGiven
    if fst lineSplit == "echo "
        then do
            putStrLn $ snd lineSplit
        else do
            putStrLn "Invalid command"
