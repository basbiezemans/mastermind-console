module Lib where

import Data.List (intersperse)
import Data.Char (intToDigit)
import Data.Maybe
import Text.Read
import Game
import Code

data Result = Correct
            | InCorrect
            deriving (Show)

getScoreVals :: Game -> (Int, Int)
getScoreVals game = (getMakerPoints m, getBreakerPoints b)
    where
        m = maker game
        b = breaker game

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
hint :: Code -> Code -> String
hint c1 c2 = hintToString $ ones ++ zeros (fst pair) (snd pair)
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

hintToString :: [Int] -> String
hintToString [] = "no matching digits"
hintToString xs = intersperse ',' $ map intToDigit xs

endOf :: Game -> Bool
endOf game = unLimit (limit game) == unCounter (counter game)

strToScore :: String -> (CodeMaker, CodeBreaker)
strToScore str = (CodeMaker m, CodeBreaker b)
    where
        safeRead s = readMaybe s :: Maybe Int
        isDuo = (== 2) . length
        score = words str
        readInt = (fromMaybe 0) . safeRead
        [m, b] = if isDuo score then map readInt score else [0, 0]