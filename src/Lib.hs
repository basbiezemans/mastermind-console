module Lib where

import Data.Maybe
import Code

type Counter = Int
type Limit = Int
type Player = Int
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
resultOf c1 c2 = if c1 == c2 then Correct else InCorrect

isCorrect :: Result -> Bool
isCorrect Correct   = True
isCorrect InCorrect = False

codeToList :: Code -> [Int]
codeToList (Code a b c d) = a : b : c : [d]

-- | Take two codes and return a pair of Int-lists with all matching digits removed
remMatches :: Code -> Code -> ([Int], [Int])
remMatches c1 c2 = remMatches' (codeToList c1, codeToList c2) 4
    where
        remMatches' pair 0 = pair
        remMatches' ((x:xs), (y:ys)) n
            | x == y       = remMatches' (xs, ys) (n - 1)
            | otherwise    = remMatches' (xs ++ [x], ys ++ [y]) (n - 1)

-- | Take two codes and return a hint which shows how many digits match and/or are included
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
