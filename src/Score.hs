module Score where

import Data.Bifunctor (bimap)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Game

type Score = (CodeMaker, CodeBreaker)

initial :: Score
initial = (CodeMaker 0, CodeBreaker 0)

toString :: Score -> String
toString score = show ( codeMaker score
                      , codeBreaker score )

fromString :: String -> Score
fromString = bimap CodeMaker CodeBreaker . scoreVals
    where
        scoreVals = fromMaybe (0,0) . safeRead
        safeRead s = readMaybe s :: Maybe (Int, Int)

codeMaker :: Score -> Int
codeMaker = getMakerPoints . fst

codeBreaker :: Score -> Int
codeBreaker = getBreakerPoints . snd
