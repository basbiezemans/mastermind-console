module Result where

import Code (Code, Guess, unGuess)

data Result = Correct | InCorrect
    deriving (Show)

resultOf :: Guess -> Code -> Result
resultOf guess code =
    if unGuess guess == code then Correct else InCorrect

isCorrect :: Result -> Bool
isCorrect Correct   = True
isCorrect InCorrect = False
