{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game
    ( Game (..)
    , Limit (..)
    , Counter (..)
    , CodeMaker (..)
    , CodeBreaker (..)
    , inc
    , makeGame
    ) where

import Code

newtype Limit = Limit
    { unLimit :: Int
    } deriving (Show)

newtype Counter = Counter
    { unCounter :: Int
    } deriving (Show, Increment)

newtype CodeMaker = CodeMaker
    { getMakerPoints :: Int
    } deriving (Show, Increment)

newtype CodeBreaker = CodeBreaker
    { getBreakerPoints :: Int
    } deriving (Show, Increment)

class Increment a where
    inc :: a -> a

instance Increment Int where
    inc = (+ 1)

data Game = Game
    { pattern :: Code
    , limit   :: Limit
    , counter :: Counter
    , maker   :: CodeMaker
    , breaker :: CodeBreaker
    }

makeGame :: Code -> Limit -> (CodeMaker, CodeBreaker) -> Game
makeGame code limit players = Game
    { pattern = code
    , limit   = limit
    , counter = Counter 1
    , maker   = fst players
    , breaker = snd players
    }
