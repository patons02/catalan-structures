module Internal where

type Permutation = [Integer]

class Catalan a where
	empty :: a
	cons :: a -> a -> a
	decons :: a -> Maybe (a, a)

--bijection :: (Catalan a, Catalan b) => a -> b
--bijection w = case decons w of
--		Nothing -> empty
--		Just (u,v) -> cons (bijection u, bijection v)

