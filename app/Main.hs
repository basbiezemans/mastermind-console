{- |
MASTERMIND

Mastermind is a code-breaking game for two players. This app simulates
the role of codemaker. As a codebreaker, you can guess the code by
entering a four digit number, where each digit is between 1 and 6. The
app will respond with a hint. This hint will be empty in case non of
the digits were guessed correctly or filled with a combination of ones
and zeros for correctly guessed digits. A one indicates that a digit
has the correct position, and zero that it doesn't.

-}
module Main (main) where

import System.Random (randomRIO)
import System.IO
import System.Exit
import System.Directory
import System.Environment (getArgs)
import Data.Maybe
import Lib
import Game
import Code
import qualified Strict

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse []            = newGame $ Limit 10
parse ["-t", n]     = newGame $ makeLimit 10 n
parse ["-h"]        = usage >> exit
parse ["-v"]        = version >> exit
parse ["--help"]    = parse ["-h"]
parse ["--version"] = parse ["-v"]
parse _             = parse ["-h"]

usage :: IO ()
usage = do
    putStrLn "\nUsage: mastermind [-t NUMBER]"
    putStrLn "\nAvailable options:"
    putStrLn "  -h, --help         Show this help text"
    putStrLn "  -v, --version      Show the version number"
    putStrLn "  -t NUMBER          Number of turns from 8-12 (default: 10)\n"

version :: IO ()
version = putStrLn "Mastermind v0.1"

exit :: IO ()
exit = exitWith ExitSuccess

newGame :: Limit -> IO ()
newGame limit = do
    putStrLn "+------------------------------------+"
    putStrLn "| Mastermind, the code-breaking game |"
    putStrLn "+------------------------------------+"
    putStr $ "You have " ++ (show $ unLimit limit)
    putStrLn " turns to guess the code. Good luck!"
    code <- generateCode
    mstr <- retrieve ".mastermind"
    play $ makeGame code limit $
        case mstr of
            Just s  -> strToScore s
            Nothing -> (CodeMaker 0, CodeBreaker 0)

newline :: IO ()
newline = putChar '\n'

askCode :: String -> IO (Maybe Code)
askCode question = do
    newline
    putStr question
    hFlush stdout
    input <- getLine
    return (makeCode input)

-- | Generate a random 4 digit code, where each digit is between 1 and 6
generateCode :: IO Code
generateCode = do
    a <- randomRIO (1,6)
    b <- randomRIO (1,6)
    c <- randomRIO (1,6)
    d <- randomRIO (1,6)
    return (Code a b c d)

play :: Game -> IO ()
play game = do
    guess <- askCode "Guess: "
    case guess of
        Nothing   -> explain game
        Just code -> continue game code

continue :: Game -> Code -> IO ()
continue game guess = do
    let result = resultOf guess $ pattern game
    if isCorrect result || endOf game then do
        let current = update game result
        store current ".mastermind"
        recap current result
    else
        evaluate game guess

explain :: Game -> IO ()
explain game = do
    putStrLn "Please enter 4 digits, where each digit is between 1 and 6, e.g. 1234"
    play game

recap :: Game -> Result -> IO ()
recap game result = do
    let (mPoints, bPoints) = getScoreVals game
    let patt = pattern game
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
        _   -> newGame $ limit game

evaluate :: Game -> Code -> IO ()
evaluate game guess = do
    let count = unCounter $ counter game
    let patt = pattern game
    putStrLn $ "Turn: #" ++ (show count)
    putStrLn $ "Hint: " ++ (hint guess patt)
    if count == 5 then do
        putStr "Hint: the sum of the digits in the code is "
        putStrLn (show $ sum $ codeToList patt)
    else
        return ()
    play $ incCounter game

store :: Game -> String -> IO ()
store game filePath = writeFile filePath $ show $ getScoreVals game

retrieve :: String -> IO (Maybe String)
retrieve filePath = do
    fileExists <- doesFileExist filePath
    if fileExists then do
        contents <- Strict.readFile filePath
        return (Just contents)
    else
        return Nothing
