module Score where

import Control.Monad (liftM2)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Game

makeScore :: Int -> Int -> Score
makeScore p1 p2 = Score
    { _codeMaker = p1
    , _codeBreaker = p2
    }

initial :: Score
initial = makeScore 0 0

toString :: Score -> String
toString = show . liftM2 (,) codeMaker codeBreaker

fromString :: String -> Score
fromString = uncurry makeScore . scoreVals
    where
        scoreVals = fromMaybe (0,0) . safeRead
        safeRead s = readMaybe s :: Maybe (Int, Int)

codeMaker :: Score -> Int
codeMaker = _codeMaker

codeBreaker :: Score -> Int
codeBreaker = _codeBreaker
