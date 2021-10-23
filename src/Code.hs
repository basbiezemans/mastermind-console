module Code (Code (Code), makeCode) where

import Data.Char
import Data.Maybe

data Code = Code Int Int Int Int
            deriving (Eq)

instance Show Code where
    show (Code a b c d) = map intToDigit [a, b, c, d]

-- | Smart constructor. Does not allow digits other than 1..6
makeCode :: String -> Maybe Code
makeCode str = if isValid guess then listToCode guess else Nothing
    where
        guess = map digitToInt $ filter isDigit str
        valid x = 0 < x && x < 7
        isValid xs = (length xs) == 4 && all valid xs

listToCode :: [Int] -> Maybe Code
listToCode (a:b:c:d:_) = Just (Code a b c d)
listToCode _ = Nothing
