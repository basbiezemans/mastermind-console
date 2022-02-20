module Lib where

import Data.List (intersperse, intersect)
import Data.Char (intToDigit)
import Data.Maybe (fromMaybe)
import Data.Bifunctor (bimap)
import Text.Read (readMaybe)
import Game
import Code

newtype Guess = Guess { unGuess :: Code }
    deriving Show

data Result = Correct | InCorrect
    deriving Show

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

resultOf :: Guess -> Code -> Result
resultOf guess code = if unGuess guess == code then Correct else InCorrect

isCorrect :: Result -> Bool
isCorrect Correct   = True
isCorrect InCorrect = False

-- | Take two codes and return a hint which shows how many digits match and/or are included
hint :: Code -> Guess -> String
hint code guess = hintToString $ ones ++ zeros
    where
        pairs = zip (toList code) (toList $ unGuess guess)
        pair = unzip $ filter (uncurry (/=)) pairs
        n = length (toList code) - length (fst pair)
        ones = replicate n 1
        m = length $ uncurry intersect pair
        zeros = replicate m 0

hintToString :: [Int] -> String
hintToString [] = "no matching digits"
hintToString xs = intersperse ',' $ map intToDigit xs

endOf :: Game -> Bool
endOf game = unLimit (limit game) == unCounter (counter game)

makeLimit :: Int -> String -> Limit
makeLimit default' str =
    Limit $ fromMaybe default' $ safeRead str
    where
        safeRead s = (readMaybe s :: Maybe Int) >>= safeValue
        safeValue x = if elem x [8..12] then Just x else Nothing

strToScore :: String -> (CodeMaker, CodeBreaker)
strToScore str = bimap CodeMaker CodeBreaker scores
    where
        scores = fromMaybe (0,0) $ safeRead str
        safeRead s = readMaybe s :: Maybe (Int, Int)