module Lib where

import Game (Result(..))
import Code (Code)

newtype Guess = Guess { unGuess :: Code }
    deriving Show

resultOf :: Guess -> Code -> Result
resultOf guess code = if unGuess guess == code then Correct else InCorrect

isCorrect :: Result -> Bool
isCorrect Correct   = True
isCorrect InCorrect = False
