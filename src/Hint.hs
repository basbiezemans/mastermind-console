module Hint
    ( Hint
    , makeHint
    ) where

import Data.List (intersperse, intersect)
import Data.Char (intToDigit)
import Code (Code, toList, Guess, unGuess)

newtype Hint = Hint [Int]

instance Show Hint where
    show (Hint []) = "no matching digits"
    show (Hint xs) = intersperse ',' $ map intToDigit xs

-- | Take a secret code and a guess, and return a hint which
-- shows how many digits are correct and/or present in the guess.
makeHint :: Code -> Guess -> Hint
makeHint code guess = Hint (ones ++ zeros)
  where
    pairs = listOfPairs code guess
    ones = replicate (numCorrect pairs) 1
    zeros = replicate (numPresent $ unequal pairs) 0

numCorrect :: Eq a => [(a, a)] -> Int
numCorrect pairs = length pairs - length (unequal pairs)

numPresent :: Eq a => [(a, a)] -> Int
numPresent = length . uncurry intersect . unzip

unequal :: Eq a => [(a, a)] -> [(a, a)]
unequal = filter (uncurry (/=))

listOfPairs :: Code -> Guess -> [(Int, Int)]
listOfPairs code guess = zip (toList code) (toList $ unGuess guess)
