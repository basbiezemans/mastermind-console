module Result
    ( Result (..)
    , resultOf
    , isCorrect
    ) where

import Code (Code, Guess, unGuess)
import Utils ((.:))

data Result = Correct | InCorrect
    deriving (Eq, Show)

fromBool :: Bool -> Result
fromBool True  = Correct
fromBool False = InCorrect

equal :: Guess -> Code -> Bool
equal = (==) . unGuess

resultOf :: Guess -> Code -> Result
resultOf = fromBool .: equal

isCorrect :: Result -> Bool
isCorrect = (== Correct)
