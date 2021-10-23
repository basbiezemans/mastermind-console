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
import Data.Maybe
import Control.Exception
import Lib
import Game
import Code

main :: IO ()
main = do
    putStrLn "A new game has been created. Good luck!"
    code <- generateCode
    mstr <- retrieve ".mastermind"
    play $ makeGame code (Limit 10) $
        case mstr of
            Just str -> strToScore str
            Nothing  -> (CodeMaker 0, CodeBreaker 0)

newline :: IO ()
newline = putChar '\n'

askCode :: String -> IO (Maybe Code)
askCode question = do
    newline
    putStr question
    hFlush stdout
    input <- getLine
    return (makeCode input)

genRandomDigit :: (Int,Int) -> IO Int
genRandomDigit range = do
    i <- randomRIO range
    return (i)

-- | Generate a random 4 digit code, where each digit is between 1 and 6
generateCode :: IO Code
generateCode = do
    a <- genRandomDigit (1,6)
    b <- genRandomDigit (1,6)
    c <- genRandomDigit (1,6)
    d <- genRandomDigit (1,6)
    return (Code a b c d)

play :: Game -> IO ()
play game = do
    guess <- askCode "Guess: "
    case guess of
        Nothing   -> showHelp game
        Just code -> continue game code

continue :: Game -> Code -> IO ()
continue game code = do
    let scores = getScoreVals game
    let patt = pattern game
    let result = resultOf code patt
    if isCorrect result || endOf game then do
        store game result ".mastermind"
        showResult result patt scores
    else
        showHint game code

showHelp :: Game -> IO ()
showHelp game = do
    putStrLn "Please enter 4 digits, where each digit is between 1 and 6, e.g. 1234"
    play game

showResult :: Result -> Code -> (Int, Int) -> IO ()
showResult result patt scores = do
    let mPoints = fst scores
    let bPoints = snd scores
    case result of
        Correct   -> putStrLn "You won!"
        InCorrect -> putStrLn $ "You lost. The answer was " ++ (show patt)
    newline
    putStr $ "The score is: " ++ (show bPoints) ++ " (You) / "
    putStrLn $ (show mPoints) ++ " (Code Maker)"
    newline
    putStr "Would you like to play again? (Y/n) (default is Y): "
    hFlush stdout
    choice <- getChar
    newline
    case choice of
        'n' -> newline
        _   -> main

showHint :: Game -> Code -> IO ()
showHint game code = do
    let count = unCounter $ counter game
    let patt = pattern game
    putStrLn $ "Turn: #" ++ (show count)
    putStrLn $ "Hint: " ++ (hint code patt)
    if count == 5 then do
        putStr "Hint: the sum of the digits in the code is "
        putStrLn (show $ sum $ codeToList patt)
    else
        return ()
    play $ incCounter game

store :: Game -> Result -> String -> IO ()
store game result filePath =
    case result of
        Correct -> writeScore $ addCodeBreakerPoint game
        InCorrect -> writeScore $ addCodeMakerPoint game
    where
        toString (m, b) = unwords $ map show [m, b]
        writeScore game = writeFile filePath $ toString $ getScoreVals game

retrieve :: String -> IO (Maybe String)
retrieve filePath = safeReadFile filePath
    where
        exception exp = const (return "") (exp :: IOException)
        safeReadFile filePath = do
            contents <- readFile filePath `catch` exception
            return (if contents == "" then Nothing else Just contents)