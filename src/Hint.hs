module Hint where

import Data.List (intersperse, intersect)
import Data.Char (intToDigit)
import Code (Code, toList, Guess, unGuess)

newtype Hint = Hint [Int]

instance Show Hint where
    show (Hint []) = "no matching digits"
    show (Hint xs) = intersperse ',' $ map intToDigit xs

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