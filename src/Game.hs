{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Game
    ( Game (..)
    , Score (..)
    , Limit (..)
    , Counter (..)
    , makeGame
    , secret
    , counter
    , value
    , score
    , config
    , endOf
    , unLimit
    , incCounter
    , addCodeMakerPoint
    , addCodeBreakerPoint
    ) where

import Control.Lens (makeLenses, view, over)
import Code (Code)

newtype Limit = Limit
    { _limit :: Int
    } deriving (Show)

newtype Counter = Counter
    { _value :: Int
    } deriving (Show)

data Score = Score
    { _codeMaker   :: Int
    , _codeBreaker :: Int
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

unLimit :: Limit -> Int
unLimit = _limit

endOf :: Game -> Bool
endOf = (==) <$> view (config . limit)
             <*> view (counter . value)

incCounter :: Game -> Game
incCounter = over (counter . value) (+1)

addCodeMakerPoint :: Game -> Game
addCodeMakerPoint = over (score . codeMaker) (+1)

addCodeBreakerPoint :: Game -> Game
addCodeBreakerPoint = over (score . codeBreaker) (+1)