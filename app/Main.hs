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
import System.IO (hFlush, stdout)
import System.Exit (exitSuccess)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Data.Maybe ()
import Control.Monad (when)
import Lib
import Game
import Code
import qualified Strict

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse args = case args of
    []            -> newGame $ Limit 10
    ["-t", n]     -> newGame $ makeLimit 10 n
    ["-h"]        -> usage >> exit
    ["-v"]        -> version >> exit
    ["--help"]    -> parse ["-h"]
    ["--version"] -> parse ["-v"]
    _             -> parse ["-h"]

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
exit = exitSuccess

splash :: IO ()
splash = do
    putStrLn "+------------------------------------+"
    putStrLn "| Mastermind, the code-breaking game |"
    putStrLn "+------------------------------------+"

turns :: Int -> IO ()
turns n = do
    putStr $ "You have " ++ show n
    putStrLn " turns to guess the code. Good luck!"

newGame :: Limit -> IO ()
newGame limit = do
    splash
    turns $ unLimit limit
    code <- generateCode
    mstr <- retrieve ".mastermind"
    play $ makeGame code limit $
        maybe (CodeMaker 0, CodeBreaker 0) strToScore mstr

newline :: IO ()
newline = putChar '\n'

askCode :: String -> IO (Maybe Code)
askCode question = do
    newline
    putStr question
    hFlush stdout
    makeCode <$> getLine

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

continue :: Game -> Guess -> IO ()
continue game guess = do
    let result = resultOf guess $ code game
    if isCorrect result || endOf game then do
        let game' = update game result
        store game' ".mastermind"
        recap game' result
    else
        evaluate game guess

explain :: Game -> IO ()
explain game = do
    putStrLn "Please enter 4 digits, where each digit is between 1 and 6, e.g. 1234"
    play game

recap :: Game -> Result -> IO ()
recap game result = do
    let (mPoints, bPoints) = getScoreVals game
    case result of
        Correct   -> putStrLn "You won!"
        InCorrect -> putStrLn $ "You lost. The answer was " ++ show (code game)
    newline
    putStr $ "The score is: " ++ show bPoints ++ " (You) / "
    putStrLn $ show mPoints ++ " (Code Maker)"
    newline
    putStr "Would you like to play again? (Y/n) (default is Y): "
    hFlush stdout
    choice <- getChar
    newline
    case choice of
        'n' -> newline
        _   -> newGame $ limit game

evaluate :: Game -> Guess -> IO ()
evaluate game guess = do
    let count = unCounter $ counter game
    putStrLn $ "Turn: #" ++ show count
    putStrLn $ "Hint: " ++ hint guess (code game)
    when (count == 5) $ do
        putStr "Hint: the sum of the digits in the code is "
        putStrLn $ show $ sum $ codeToList (code game)
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
