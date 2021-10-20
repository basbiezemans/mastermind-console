module Game
    ( Game (..)
    , Counter (..)
    , Limit (..)
    , CodeMaker (..)
    , CodeBreaker (..)
    , makeGame ) where

import Code

newtype Counter = Counter
    { unCounter :: Int
    } deriving (Show)

newtype Limit = Limit
    { unLimit :: Int
    } deriving (Show)

newtype CodeMaker = CodeMaker
    { unCodeMaker :: Int
    } deriving (Show)

newtype CodeBreaker = CodeBreaker
    { unCodeBreaker :: Int
    } deriving (Show)

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
