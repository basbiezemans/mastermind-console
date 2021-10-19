{- |
MASTERMIND

Mastermind is a code-breaking game for two players. This app simulates
the role of codemaker. As a codebreaker, you can guess the code by
entering a four digit number, where each digit is between 1 and 6. The
app will respond with a hint. This hint will be empty in case non of
the digits were guessed correctly or filled with a combination of ones
and zeros for correctly guessed digits. One indicates that a digit has
the correct position, and zero that it doesn't.

-}
module Main (main) where

import System.Random (randomRIO)
import System.IO
import Data.Maybe (fromJust, isNothing)
import Lib

main :: IO ()
main = do
    putStrLn "A new game has been created. Good luck!"
    -- Create a new game with a maximum of 10 turns 
    code <- generateCode
    play $ newGame code 10

newline :: IO ()
newline = putChar '\n'

genRandomDigit :: (Int,Int) -> IO Int
genRandomDigit range = do
    i <- randomRIO range
    return (i)

-- Generate a random 4 digit code, where each digit is between 1 and 6
generateCode :: IO Code
generateCode = do
    a <- genRandomDigit (1,6)
    b <- genRandomDigit (1,6)
    c <- genRandomDigit (1,6)
    d <- genRandomDigit (1,6)
    return (a, b, c, d)

play :: Game -> IO ()
play game = do
    newline
    putStr "Guess: "
    hFlush stdout
    input <- getLine
    let guess = fromInput input
    if isNothing guess then do
        putStrLn "Please enter 4 digits, where each digit is between 1 and 6, e.g. 1234"
        play game
    else do
        let code = fromJust guess
        let pattern = getPattern game
        let result = resultOf code pattern
        if isCorrect result || endOf game then do
            case result of
                Correct   -> putStrLn "You won!"
                InCorrect -> putStrLn $ "You lost. The code was " ++ (show code)
            newline
            putStr "Would you like to play again? (Y/n) (default is Y): "
            hFlush stdout
            choice <- getChar
            newline
            case choice of
                'n' -> newline
                _   -> main
        else do
            let counter = getCounter game
            putStr "Turn: #"
            putStrLn (show counter)
            putStr "Hint: "
            putStrLn (show $ hint code pattern)
            if counter == 5 then do
                putStr "Hint: the sum of the digits in the code is "
                putStrLn (show $ sum $ codeToList pattern)
            else return ()
            play $ incCounter game
