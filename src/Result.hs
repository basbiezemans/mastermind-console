module Result
    ( Result (..)
    , resultOf
    , isCorrect
    ) where

import Code (Code, Guess, unGuess)
import Utils ((.:))

data Result = Correct | InCorrect
    deriving (Show)

result :: Bool -> Result
result True  = Correct
result False = InCorrect

equal :: Guess -> Code -> Bool
equal = (==) . unGuess

resultOf :: Guess -> Code -> Result
resultOf = result .: equal

isCorrect :: Result -> Bool
isCorrect Correct   = True
isCorrect InCorrect = False
