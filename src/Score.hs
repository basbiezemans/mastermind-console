module Score where

import Control.Monad (liftM2)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

data Score = Score
    { _codeMaker   :: Int
    , _codeBreaker :: Int
    }

instance Show Score where
    show = show . liftM2 (,) _codeMaker _codeBreaker

makeScore :: Int -> Int -> Score
makeScore p1 p2 = Score
    { _codeMaker = p1
    , _codeBreaker = p2
    }

initial :: Score
initial = makeScore 0 0

fromString :: String -> Score
fromString = uncurry makeScore . scoreVals
  where
    scoreVals = fromMaybe (0,0) . safeRead
    safeRead s = readMaybe s :: Maybe (Int, Int)
