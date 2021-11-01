module Lib where

import Data.List (intersperse, delete)
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

update :: Game -> Result -> Game
update game result =
    case result of
        Correct -> addCodeBreakerPoint game
        InCorrect -> addCodeMakerPoint game

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
codeToList (Code a b c d) = [a, b, c, d]

codeLen :: Int
codeLen = 4

-- | Take two codes and return a pair of Int-lists with all matching digits removed
remMatches :: Code -> Code -> ([Int], [Int])
remMatches c1 c2 = remMatches' (codeToList c1, codeToList c2) codeLen
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
        len = codeLen - (length $ fst pair)
        ones = replicate len 1
        zeros [] _      = []
        zeros (x:xs) ys
            | elem x ys = 0 : (zeros xs $ delete x ys)
            | otherwise = zeros xs ys

hintToString :: [Int] -> String
hintToString [] = "no matching digits"
hintToString xs = intersperse ',' $ map intToDigit xs

endOf :: Game -> Bool
endOf game = unLimit (limit game) == unCounter (counter game)

makeLimit :: Int -> String -> Limit
makeLimit default' str =
    case safeVal str of
        Just n  -> Limit n
        Nothing -> Limit default'
    where
        safeVal s = (readMaybe s :: Maybe Int) >>= safeInt
        safeInt x = if elem x [8..12] then Just x else Nothing

strToScore :: String -> (CodeMaker, CodeBreaker)
strToScore str = (CodeMaker $ fst scores, CodeBreaker $ snd scores)
    where
        scores = fromMaybe (0,0) $ safeRead str
        safeRead s = readMaybe s :: Maybe (Int, Int)