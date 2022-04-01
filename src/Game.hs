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
    , codeMaker
    , codeBreaker
    , addCodeMakerPoint
    , addCodeBreakerPoint
    ) where

import Control.Monad (liftM2)
import Lens.Micro (over)
import Lens.Micro.Extras (view)
import Lens.Micro.TH (makeLenses)
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
    }

instance Show Score where
    show = show . liftM2 (,) _codeMaker _codeBreaker

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