module Internal where

class Catalan a where
	cons :: a -> a -> a
	decons :: a -> (a, a)
