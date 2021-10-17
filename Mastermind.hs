{----------------------------------------------------------------------
 MASTERMIND

 Mastermind is a code-breaking game for two players. This app simulates
 the role of codemaker. As a codebreaker, you can guess the code by
 entering a four digit number, where each digit is between 1 and 6. The
 app will respond with a hint. This hint will be empty in case non of
 the digits were guessed correctly or filled with a combination of ones
 and zeros for correctly guessed digits. One indicates that a digit has
 the correct position, and zero that it doesn't.
 -}

module Main(main) where

import System.Random
import System.IO
import Data.Char
import Data.Maybe


type Counter = Int
type Limit = Int
type Player = Int
type Code = [Int]
type Hint = [Int]

data Result = Correct
            | InCorrect
            deriving (Show)

data Game = Game
    { pattern :: Code
    , limit   :: Limit
    , counter :: Counter
    , maker   :: Player
    , breaker :: Player
    }


newGame :: Code -> Limit -> Game
newGame code limit = Game
    { pattern = code
    , limit   = limit
    , counter = 1
    , maker   = 0
    , breaker = 0
    }

getPattern :: Game -> Code
getPattern = pattern

getLimit :: Game -> Counter
getLimit = limit

getCounter :: Game -> Counter
getCounter = counter

getScore :: Game -> (Player, Player)
getScore game = (maker game, breaker game)

incCounter :: Game -> Game
incCounter game = game { counter = inc $ counter game }

addCodeMakerPoint :: Game -> Game
addCodeMakerPoint game = game { maker = inc $ maker game }

addCodeBreakerPoint :: Game -> Game
addCodeBreakerPoint game = game { breaker = inc $ breaker game }

resultOf :: Code -> Code -> Result
resultOf xs ys = if xs == ys then Correct else InCorrect

isCorrect :: Result -> Bool
isCorrect Correct   = True
isCorrect InCorrect = False

hint :: Code -> Code -> Hint
hint xs ys = ones ++ zeros (fst pair) (snd pair)
    where
        -- Reduce (xs, ys) to a pair with all correct digits removed
        reducer ((x:xs), (y:ys)) _
            | x == y    = (xs, ys)
            | otherwise = (xs ++ [x], ys ++ [y])
        pair = foldl reducer (xs, ys) [0..3]
        len = 4 - (length $ fst pair)
        ones = replicate len 1
        zeros [] _      = []
        zeros (x:xs) ys
            | elem x ys = (zeros xs (remove x ys)) ++ [0]
            | otherwise = (zeros xs ys)
            where
                remove e xs = filter (/= e) xs

inc :: Int -> Int
inc = (+ 1)

endOf :: Game -> Bool
endOf game = limit game == counter game

update :: Game -> Result -> Game
update game Correct   = addCodeBreakerPoint game
update game InCorrect = if endOf game then addCodeMakerPoint game else game

fromInput :: String -> Maybe Code
fromInput xs = if isValid code then Just code else Nothing
    where
        code = strToCode xs
        valid x = 0 < x && x < 7
        isValid xs = (length xs) == 4 && all valid xs

strToCode :: String -> Code
strToCode []                 = []
strToCode (x:xs) | isDigit x = [digitToInt x] ++ strToCode xs
                 | otherwise = strToCode xs

newline :: IO ()
newline = putChar '\n'

genRandomDigit :: (Int,Int) -> IO Int
genRandomDigit range = do
    i <- randomRIO range
    return (i)

{----------------------------------------------------------------------
 OUTPUT BUFFERING

 GHCi and the compiled program do not buffer the output in quite the
 same way. In the compiled program, stdout is line buffered, so it
 will not output anything until it gets a newline character. You can
 force the output using 'hFlush stdout'.

 https://mail.haskell.org/pipermail/haskell/2006-September/018430.html
 -}

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
        let counter = getCounter game
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
            putStr "Turn: #"
            putStrLn (show counter)
            putStr "Hint: "
            putStrLn (show $ hint code pattern)
            if counter == 5 then do
                putStr "Hint: the sum of the digits in the code is "
                putStrLn (show $ sum pattern)
            else return ()
            play $ incCounter game

main :: IO ()
main = do
    -- Generate a random 4 digit code, where each
    -- digit is between 1 and 6
    code <- mapM genRandomDigit $ replicate 4 (1,6)
    -- Maximum number of turns 
    let limit = 10
    putStrLn "A new game has been created. Good luck!"
    play $ newGame code limit
