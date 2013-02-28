{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- Module      : Math.CatalanStructures.DyckPath
-- Copyright   : (c) Stuart Paton 2013
-- License     : undecided
-- Maintainer  : Stuart Paton <stuart.john.paton@gmail.com>
-- 

module StackSortPerm where

import qualified Math.Sym as S
import qualified Math.Sym.Plot as P 

import Internal

data StackSortablePermutation = Empty | Perm231 Permutation deriving (Eq, Ord, Show)

--type StackSortablePermutation = Permutation

instance Catalan StackSortablePermutation where
	empty = Empty
	cons = mkIndec
	decons = decompose

{-----------------------------------------------------------
	Utility functions
------------------------------------------------------------}

mkIndec :: StackSortablePermutation -> StackSortablePermutation -> StackSortablePermutation
mkIndec alpha beta = Perm231 (a ++ [n] ++ b)
	where
	n = length (a ++ b) + 1
	a = ssptoperm alpha
	b = ssptoperm beta

decompose :: StackSortablePermutation -> Maybe (StackSortablePermutation, StackSortablePermutation)
decompose sigma = if (S.avoids (sspToString sigma) ["231"]) || (S.avoids (sspToString sigma) ["132"])
		  then Just (pairTossp $ removeHeadSnd $ break (l ==) s)
		  else Nothing
	where
	l = length s
	s = ssptoperm sigma

removeHeadSnd :: (t, [a]) -> (t, [a])
removeHeadSnd (alpha, beta) = (alpha, tail beta)

(<+>) :: Permutation -> Permutation -> Permutation
pi <+> rho =  pi ++ rho' rho
	where
	rho' [] = []
	rho' (x:xs) = length pi + x : rho' xs 

alpha :: Permutation -> StackSortablePermutation
alpha pi = Perm231 (n : pi)
	where
	n = length pi + 1

sspToString :: StackSortablePermutation -> String
sspToString p = permToString $ ssptoperm p

permTossp :: Permutation -> StackSortablePermutation
permTossp p = case p of
	[] -> Empty
	a -> Perm231 a

ssptoperm :: StackSortablePermutation -> Permutation
ssptoperm p = case p of
	Empty -> []
	Perm231 a -> a

pairTossp :: ([Int], [Int]) -> (StackSortablePermutation, StackSortablePermutation)
pairTossp (a,b) = (Perm231 a, Perm231 b)

{-----------------------------------------------------------
	Graphics functions
------------------------------------------------------------}

draw :: StackSortablePermutation -> IO ()
draw perm = drawPerm $ ssptoperm perm
