module Utils where

-- | Ensures that a maybe value satisfies a given predicate
ensure :: (a -> Bool) -> a -> Maybe a
ensure p v | p v       = Just v
           | otherwise = Nothing
