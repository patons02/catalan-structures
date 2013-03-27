module Math.Av321 where
-- Module      : Math.Av321
-- Copyright   : (c) Stuart Paton 2013
-- License     : BSD3
-- Maintainer  : Stuart Paton <stuart.john.paton@gmail.com>
-- 
import Math.Internal
import qualified Math.Av123 as P


data Perm321 = Empty | Perm321 Permutation deriving (Eq, Ord, Show)

get321 :: P.Perm123 -> Perm321
get321 p = case p of
	P.Empty -> Empty
	P.Perm p' -> Perm321 $ revPerm p'

get123 :: Perm321 -> P.Perm123
get123 p = case p of
	Empty -> P.Empty
	Perm321 p' -> P.Perm $ revPerm p'

{-----------------------------------------------------------
	Utility Functions
------------------------------------------------------------}

perm321ToString :: Perm321 -> String
perm321ToString p = permToString $ perm321toperm p

permToperm321 :: Permutation -> Perm321
permToperm321 p = case p of
	[] -> Empty
	a -> Perm321 a

perm321toperm :: Perm321 -> Permutation
perm321toperm p = case p of
	Empty -> []
	Perm321 a -> a

{-----------------------------------------------------------
	Graphics functions
------------------------------------------------------------}

draw :: Perm321 -> IO ()
draw p = drawPerm $ perm321toperm p

