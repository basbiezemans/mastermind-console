module Lib where

import Data.Char
import Data.Maybe

type Counter = Int
type Limit = Int
type Player = Int
type Hint = [Int]
type Code = (Int, Int, Int, Int)

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

codeToList :: Code -> [Int]
codeToList (a,b,c,d) = a : b : c : [d]

listToCode :: [Int] -> Maybe Code
listToCode (a:b:c:d:_) = Just (a, b, c, d)
listToCode _ = Nothing

-- Take two codes and return a pair if Int-lists with all matching digits removed
remMatches :: Code -> Code -> ([Int], [Int])
remMatches c1 c2 = remMatches' (codeToList c1, codeToList c2) 4
    where
        remMatches' pair 0 = pair
        remMatches' ((x:xs), (y:ys)) n
            | x == y       = remMatches' (xs, ys) (n - 1)
            | otherwise    = remMatches' (xs ++ [x], ys ++ [y]) (n - 1)

hint :: Code -> Code -> Hint
hint c1 c2 = ones ++ zeros (fst pair) (snd pair)
    where
        pair = remMatches c1 c2
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
fromInput str = if isValid guess then listToCode guess else Nothing
    where
        guess = strToIntList str
        valid x = 0 < x && x < 7
        isValid xs = (length xs) == 4 && all valid xs

strToIntList :: String -> [Int]
strToIntList []                 = []
strToIntList (x:xs) | isDigit x = digitToInt x : strToIntList xs
                    | otherwise = strToIntList xs
