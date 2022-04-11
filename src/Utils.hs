module Utils where

-- | Ensures that a maybe value satisfies a given predicate
ensure :: (a -> Bool) -> a -> Maybe a
ensure p v | p v       = Just v
           | otherwise = Nothing

-- | Apply a single function to both components of a pair
both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

-- | B1 combinator: λabcd.a(bcd)
(.:), blackbird :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:)      = blackbird
blackbird = (.) . (.)

infixr 8 .:
