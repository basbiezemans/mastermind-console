module Code
    ( Guess (..)
    , Code
    , fromString
    , toList
    , fromList
    ) where

import Data.Char (digitToInt, intToDigit, isDigit)
import Data.Maybe ()

data Code = Code Int Int Int Int
            deriving (Eq)

instance Show Code where
    show = map intToDigit . toList

newtype Guess = Guess { unGuess :: Code }
    deriving Show

fromString :: String -> Maybe Code
fromString = fromList . map digitToInt . selectDigits

toList :: Code -> [Int]
toList (Code a b c d) = [a, b, c, d]

fromList :: [Int] -> Maybe Code
fromList [a,b,c,d] = Just (Code a b c d)
fromList _         = Nothing

selectDigits :: [Char] -> [Char]
selectDigits = filter isValidDigit

isValidDigit :: Char -> Bool
isValidDigit x = isDigit x && elem x ['1'..'6']
