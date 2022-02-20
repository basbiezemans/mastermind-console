module Code
    ( Code
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

-- | Smart constructor. Does not allow digits other than 1..6
fromString :: String -> Maybe Code
fromString = fromList . map digitToInt . selectDigits
    where
        selectDigits = filter isValidDigit
        isValidDigit x = isDigit x && elem x ['1'..'6']
        
toList :: Code -> [Int]
toList (Code a b c d) = [a, b, c, d]

fromList :: [Int] -> Maybe Code
fromList [a,b,c,d] = Just (Code a b c d)
fromList _         = Nothing
