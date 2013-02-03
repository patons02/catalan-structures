module Internal where

type Permutation = [Integer]

class Catalan a where
	cons :: a -> a -> a
	decons :: a -> (a, a)

