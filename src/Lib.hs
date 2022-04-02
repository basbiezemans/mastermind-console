module Lib where

import Data.List (intersperse, intersect)
import Data.Char (intToDigit)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Game
import Code

newtype Guess = Guess { unGuess :: Code }
    deriving Show

newtype Hint = Hint [Int]

instance Show Hint where
    show (Hint []) = "no matching digits"
    show (Hint xs) = intersperse ',' $ map intToDigit xs

resultOf :: Guess -> Code -> Result
resultOf guess code = if unGuess guess == code then Correct else InCorrect

isCorrect :: Result -> Bool
isCorrect Correct   = True
isCorrect InCorrect = False

-- | Take a secret code and a guess, and return a hint which
-- shows how many digits match and/or are included in the guess.
makeHint :: Code -> Guess -> Hint
makeHint code guess = Hint (ones ++ zeros)
  where
    pairs = zip (toList code) (toList $ unGuess guess)
    pair = unzip $ filter (uncurry (/=)) pairs
    n = length (toList code) - length (fst pair)
    ones = replicate n 1
    m = length $ uncurry intersect pair
    zeros = replicate m 0

makeLimit :: Int -> String -> Limit
makeLimit default' str =
    Limit $ fromMaybe default' $ safeRead str
  where
    safeRead s = (readMaybe s :: Maybe Int) >>= safeValue
    safeValue x = if elem x [8..12] then Just x else Nothing
