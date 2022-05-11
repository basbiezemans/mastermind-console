module Main (main) where

import System.Random (randomRIO)
import System.IO (hFlush, stdout)
import System.IO.Strict as Strict (readFile)
import System.Exit (exitSuccess)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Data.Maybe (fromJust)
import Lens.Micro ((^.))
import Control.Monad (when, replicateM)
import Hint (makeHint)
import Code
import Game
import Limit
import Result
import qualified Score

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse args = case args of
    []            -> newGame $ Limit 10
    ["-t", n]     -> newGame $ limitOr 10 (safeValue n)
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
    splash >> turns (unLimit limit)
    code <- generateCode
    mstr <- retrieveScore
    play $ makeGame code limit $
        maybe Score.initial Score.fromString mstr

newline :: IO ()
newline = putChar '\n'

askCode :: IO (Maybe Code)
askCode = do
    newline
    putStr "Guess: "
    hFlush stdout
    Code.fromString <$> getLine

-- | Generate a random 4 digit code, where each digit is between 1 and 6
generateCode :: IO Code
generateCode = do
    rInts <- replicateM 4 (randomRIO (1,6) :: IO Int)
    return (fromJust $ Code.fromList rInts)

play :: Game -> IO ()
play game = do
    askCode >>= maybe (explain game)
                      (continue game . Guess)

continue :: Game -> Guess -> IO ()
continue game guess = do
    let result = resultOf guess (game ^. secret)
    if isCorrect result || endOf game then
        store (update game result) >>= recap result
    else
        evaluate game guess

explain :: Game -> IO ()
explain game = do
    putStrLn "Please enter 4 digits, where each digit is between 1 and 6, e.g. 1234"
    play game

recap :: Result -> Game -> IO ()
recap result game = do
    showWinner result (game ^. secret)
    newline
    putStr "The score is: "
    putStr $ show (game ^. score . codeBreaker) ++ " (You) / "
    putStrLn $ show (game ^. score . codeMaker) ++ " (Code Maker)"
    newline
    askPlayAgain game

showWinner :: Show a => Result -> a -> IO ()
showWinner result answer =
    putStrLn $ if isCorrect result then youWon else youLost
  where
    youWon = "You won!"
    youLost = "You lost. The answer was " ++ show answer

askPlayAgain :: Game -> IO ()
askPlayAgain game = do
    putStr "Would you like to play again? (Y/n) (default is Y): "
    hFlush stdout
    newline
    getChar >>= playAgain
  where
    playAgain 'n' = newline
    playAgain  _  = newGame (game ^. config)

evaluate :: Game -> Guess -> IO ()
evaluate game guess = do
    putStrLn $ "Turn: #" ++ show turn
    putStrLn $ "Hint: " ++ show hint
    when (turn == midgame) $ do
        putStr "Hint: the sum of the digits in the code is "
        print (sum digits)
    play $ incCounter game
  where
    turn = game ^. counter . value
    hint = makeHint (game ^. secret) guess
    digits = Code.toList (game ^. secret)
    midgame = unLimit (game ^. config) `div` 2

filePath :: String
filePath = ".mastermind"

store :: Game -> IO Game
store game = do
    writeFile filePath (show $ game ^. score)
    return game

retrieveScore :: IO (Maybe String)
retrieveScore = do
    fileExists <- doesFileExist filePath
    if fileExists then do
        contents <- Strict.readFile filePath
        return (Just contents)
    else
        return Nothing
