{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- Module      : Math.CatalanStructures.DyckPath
-- Copyright   : (c) Stuart Paton 2013
-- License     : undecided
-- Maintainer  : Stuart Paton <stuart.john.paton@gmail.com>
-- 

module StackSortPerm where

import Data.Char 

import Internal
import qualified Math.Sym as S

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

--In O(n) time complexity!
permToString :: Permutation -> String
permToString xs = foldr ((++) . show) "" xs

--below is the inefficient version of permToString in O(n^2) time complexity!
--permToString [] = ""
--permToString (x:xs) = show x ++ permToString xs

stringToPerm :: String -> Permutation
stringToPerm s = xs --map toInteger xs
	where
	xs = map digitToInt s

displayPerm :: Permutation -> IO()
displayPerm ssp = undefined



