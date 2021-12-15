module Lib where

import Data.List (intersperse, delete)
import Data.Char (intToDigit)
import Data.Maybe (fromMaybe)
import Data.Bifunctor (bimap)
import Text.Read (readMaybe)
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

-- | Take two codes and return a hint which shows how many digits match and/or are included
hint :: Code -> Code -> String
hint c1 c2 = hintToString $ ones ++ uncurry zeros pair
    where
        pairs = zip (codeToList c1) (codeToList c2)
        pair = unzip $ filter (uncurry (/=)) pairs
        len = codeLen - length (fst pair)
        ones = replicate len 1
        zeros [] _      = []
        zeros (x:xs) ys
            | elem x ys = 0 : zeros xs (delete x ys)
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
strToScore str = bimap CodeMaker CodeBreaker scores
    where
        scores = fromMaybe (0,0) $ safeRead str
        safeRead s = readMaybe s :: Maybe (Int, Int)