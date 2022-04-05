{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Game
    ( Result (..)
    , Score (..)
    , Game
    , makeGame
    , update
    , secret
    , counter
    , value
    , score
    , config
    , endOf
    , incCounter
    , codeMaker
    , codeBreaker
    ) where

import Lens.Micro (over)
import Lens.Micro.Extras (view)
import Lens.Micro.TH (makeLenses)
import Code (Code)
import Limit (Limit(..))
import Score (Score(..))
import Result (Result(..))

newtype Counter = Counter
    { _value :: Int
    } deriving (Show)

data Game = Game
    { _secret  :: Code
    , _score   :: Score
    , _config  :: Limit
    , _counter :: Counter
    } deriving (Show)

makeLenses ''Game
makeLenses ''Score
makeLenses ''Limit
makeLenses ''Counter

makeGame :: Code -> Limit -> Score -> Game
makeGame _code _config _score = Game
    { _secret  = _code
    , _score   = _score
    , _config  = _config
    , _counter = Counter 1
    }

update :: Game -> Result -> Game
update game result =
    case result of
        Correct -> addCodeBreakerPoint game
        InCorrect -> addCodeMakerPoint game

endOf :: Game -> Bool
endOf = (==) <$> view (config . limit)
             <*> view (counter . value)

incCounter :: Game -> Game
incCounter = over (counter . value) (+1)

addCodeMakerPoint :: Game -> Game
addCodeMakerPoint = over (score . codeMaker) (+1)

addCodeBreakerPoint :: Game -> Game
addCodeBreakerPoint = over (score . codeBreaker) (+1)
