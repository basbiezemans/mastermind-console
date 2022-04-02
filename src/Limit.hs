module Limit where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

newtype Limit = Limit
    { _limit :: Int
    } deriving (Show)

unLimit :: Limit -> Int
unLimit = _limit

makeLimit :: Int -> String -> Limit
makeLimit default' str =
    Limit $ fromMaybe default' $ safeRead str
  where
    safeRead s = (readMaybe s :: Maybe Int) >>= safeValue
    safeValue x = if elem x [8..12] then Just x else Nothing