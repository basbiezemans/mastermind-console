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

newtype CodeMaker = CodeMaker Int
    deriving (Show)

newtype CodeBreaker = CodeBreaker Int
    deriving (Show)

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

makeGame :: Code -> Limit -> Game
makeGame code limit = Game
    { pattern = code
    , limit   = limit
    , counter = Counter 1
    , maker   = CodeMaker 0
    , breaker = CodeBreaker 0
    }
