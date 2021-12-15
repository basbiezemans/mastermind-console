module Code
    ( Code (Code)
    , makeCode
    , codeLen
    , codeToList
    ) where

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
        valid x = elem x [1..6]
        isValid xs = length xs == codeLen && all valid xs

codeLen :: Int
codeLen = 4

codeToList :: Code -> [Int]
codeToList (Code a b c d) = [a, b, c, d]

listToCode :: [Int] -> Maybe Code
listToCode (a:b:c:d:_) = Just (Code a b c d)
listToCode _ = Nothing
