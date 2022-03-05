module Lib where

import Data.List (intersperse, intersect)
import Data.Char (intToDigit)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Game
import Code

newtype Guess = Guess { unGuess :: Code }
    deriving Show

data Result = Correct | InCorrect
    deriving Show

update :: Game -> Result -> Game
update game result =
    case result of
        Correct -> addCodeBreakerPoint game
        InCorrect -> addCodeMakerPoint game

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

makeLimit :: Int -> String -> Limit
makeLimit default' str =
    Limit $ fromMaybe default' $ safeRead str
    where
        safeRead s = (readMaybe s :: Maybe Int) >>= safeValue
        safeValue x = if elem x [8..12] then Just x else Nothing
