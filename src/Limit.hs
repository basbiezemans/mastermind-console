module Limit
    ( Limit (..)
    , unLimit
    , limitOr
    , safeValue
    ) where

import Text.Read (readMaybe)
import Utils (ensure)

newtype Limit = Limit
    { _limit :: Int
    } deriving (Show)

unLimit :: Limit -> Int
unLimit = _limit

limitOr :: Int -> Maybe Int -> Limit
limitOr _  (Just x) = Limit x
limitOr def Nothing = Limit def

safeValue :: String -> Maybe Int
safeValue s = readMaybe s >>= ensure (`elem` [8..12])
