module Game
    ( Game (..)
    , Counter
    , Limit
    , Player
    , makeGame ) where

import Code

type Counter = Int
type Limit = Int
type Player = Int

data Game = Game
    { pattern :: Code
    , limit   :: Limit
    , counter :: Counter
    , maker   :: Player
    , breaker :: Player
    }

makeGame :: Code -> Limit -> Game
makeGame code limit = Game
    { pattern = code
    , limit   = limit
    , counter = 1
    , maker   = 0
    , breaker = 0
    }
