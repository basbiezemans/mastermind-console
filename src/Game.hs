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
    } deriving (Show)

newtype CodeMaker = CodeMaker
    { getMakerPoints :: Int
    } deriving (Show)

newtype CodeBreaker = CodeBreaker
    { getBreakerPoints :: Int
    } deriving (Show)

class Incrementable a where
    inc :: a -> a

instance Incrementable Counter where
    inc (Counter n) = Counter (n + 1)

instance Incrementable CodeMaker where
    inc (CodeMaker n) = CodeMaker (n + 1)

instance Incrementable CodeBreaker where
    inc (CodeBreaker n) = CodeBreaker (n + 1)

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
