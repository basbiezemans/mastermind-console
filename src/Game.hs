module Game
    ( Game (..)
    , Limit (..)
    , Counter
    , CodeMaker
    , CodeBreaker
    , makeGame
    , unCounter
    ) where

import Code

newtype Limit = Limit
    { unLimit :: Int
    } deriving (Show)

data Counter a = Counter a
    deriving (Show)

data CodeMaker a = CodeMaker a
    deriving (Show)

data CodeBreaker a = CodeBreaker a
    deriving (Show)

instance Functor Counter where
    fmap f (Counter x) = Counter (f x)

instance Functor CodeMaker where
    fmap f (CodeMaker x) = CodeMaker (f x)

instance Functor CodeBreaker where
    fmap f (CodeBreaker x) = CodeBreaker (f x)

data Game = Game
    { pattern :: Code
    , limit   :: Limit
    , counter :: Counter Int
    , maker   :: CodeMaker Int
    , breaker :: CodeBreaker Int
    }

makeGame :: Code -> Limit -> Game
makeGame code limit = Game
    { pattern = code
    , limit   = limit
    , counter = Counter 1
    , maker   = CodeMaker 0
    , breaker = CodeBreaker 0
    }

unCounter :: Counter Int -> Int
unCounter (Counter x) = x
