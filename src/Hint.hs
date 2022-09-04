module Hint
    ( Hint
    , makeHint
    ) where

import Data.List (intersperse, delete)
import Data.Char (intToDigit)
import Code (Code, toList, Guess, unGuess)
import Utils ((.:), both)

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
numPresent pairs =
    fst $ foldr countNumPresent (0, xs) ys
    where
        (xs, ys) = unzip pairs

countNumPresent :: Eq a => a -> (Int, [a]) -> (Int, [a])
countNumPresent y (n, xs) =
    if y `elem` xs then
        (n + 1, delete y xs)
    else
        (n, xs)

unequal :: Eq a => [(a, a)] -> [(a, a)]
unequal = filter (uncurry (/=))

listOfPairs :: Code -> Guess -> [(Int, Int)]
listOfPairs = uncurry zip .: pairOfLists

pairOfLists :: Code -> Guess -> ([Int], [Int])
pairOfLists code guess = both toList (code, unGuess guess)
