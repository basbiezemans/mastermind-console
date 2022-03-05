{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game
    ( Game (..)
    , Limit (..)
    , Counter (..)
    , CodeMaker (..)
    , CodeBreaker (..)
    , players
    , makeGame
    , endOf
    , incCounter
    , addCodeMakerPoint
    , addCodeBreakerPoint
    ) where

import Code (Code)

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
    { code    :: Code
    , limit   :: Limit
    , counter :: Counter
    , maker   :: CodeMaker
    , breaker :: CodeBreaker
    }

type Players = (CodeMaker, CodeBreaker)

makeGame :: Code -> Limit -> Players -> Game
makeGame code limit players = Game
    { code    = code
    , limit   = limit
    , counter = Counter 1
    , maker   = fst players
    , breaker = snd players
    }

players :: Game -> Players
players game = (maker game, breaker game)

endOf :: Game -> Bool
endOf game = unLimit (limit game) == unCounter (counter game)

incCounter :: Game -> Game
incCounter game = game { counter = inc $ counter game }

addCodeMakerPoint :: Game -> Game
addCodeMakerPoint game = game { maker = inc $ maker game }

addCodeBreakerPoint :: Game -> Game
addCodeBreakerPoint game = game { breaker = inc $ breaker game }