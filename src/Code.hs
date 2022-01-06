module Code
    ( Code (Code)
    , makeCode
    , codeToList
    ) where

import Data.Char (digitToInt, intToDigit, isDigit)
import Data.Maybe ()

data Code = Code Int Int Int Int
            deriving (Eq)

instance Show Code where
    show (Code a b c d) = map intToDigit [a, b, c, d]

-- | Smart constructor. Does not allow digits other than 1..6
makeCode :: String -> Maybe Code
makeCode = listToCode . map digitToInt . selectDigits
    where
        selectDigits = filter isValidDigit
        isValidDigit x = isDigit x && elem x ['1'..'6']
        
codeToList :: Code -> [Int]
codeToList (Code a b c d) = [a, b, c, d]

listToCode :: [Int] -> Maybe Code
listToCode [a,b,c,d] = Just (Code a b c d)
listToCode _         = Nothing
