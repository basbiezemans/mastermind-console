module Lib where

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
