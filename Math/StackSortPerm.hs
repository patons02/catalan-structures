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

type StackSortablePermutation = Permutation

instance Catalan StackSortablePermutation where
	empty = []
	cons = mkIndec
	decons = decompose

{-----------------------------------------------------------
	Utility functions
------------------------------------------------------------}

mkIndec :: StackSortablePermutation -> StackSortablePermutation -> StackSortablePermutation
mkIndec alpha beta = alpha ++ [n] ++ beta
	where
	n = length (alpha ++ beta) + 1

decompose :: StackSortablePermutation -> Maybe (StackSortablePermutation, StackSortablePermutation)
decompose sigma = if S.avoids (permToString sigma) ["231"]
		  then Just (removeHeadSnd $ break (l ==) sigma)
		  else Nothing
	where
	l = length sigma

removeHeadSnd :: (t, [a]) -> (t, [a])
removeHeadSnd (alpha, beta) = (alpha, tail beta)

(<+>) :: Permutation -> Permutation -> Permutation
pi <+> rho =  pi ++ rho' rho
	where
	rho' [] = []
	rho' (x:xs) = length pi + x : rho' xs 

alpha :: Permutation -> StackSortablePermutation
alpha pi = n : pi
	where
	n = length pi + 1

{-----------------------------------------------------------
	Graphics functions
------------------------------------------------------------}

draw :: StackSortablePermutation -> IO ()
draw perm = drawPerm perm
