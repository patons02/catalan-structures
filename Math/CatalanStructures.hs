-- Module      : Math.CatalanStructures
-- Copyright   : (c) Stuart Paton 2013
-- License     : undecided
-- Maintainer  : Stuart Paton <stuart.john.paton@gmail.com>
-- 

module CatalanStructures where

import Internal
import DyckPath
import StackSortPerm

{-----------------------------------------------------------------------------------
	Bijections between structures.
------------------------------------------------------------------------------------}
-- Standard bijection
-- Reference: Classification of Bijections between 321- and 132- avoiding permutations,
--	      Anders Claesson and Sergey Kitaev, 2008
ssp2dp :: StackSortablePermutation -> DyckPath
ssp2dp [] = []
ssp2dp ssp = [U] ++ ssp2dp (red (alpha, beta)) ++ [D] ++ ssp2dp beta
	where
	(alpha, beta) = stripMaybe $ decons ssp


{-----------------------------------------------------------------------------------
	Helper functions.
------------------------------------------------------------------------------------}
stripMaybe :: Maybe (a, a) -> (a, a)	
stripMaybe (Just xs) = xs

--reducation function
red :: (StackSortablePermutation, StackSortablePermutation) -> StackSortablePermutation
red ([], beta) = []
red (x:xs, beta) = x - pi_r : red (xs, beta)
	where
	pi_r = length beta


{-old version
red :: (Permutation, Permutation) -> Permutation
red ([], beta) = fst ([], beta)
red ((x:xs), beta) = fst (x - pi_r : red (xs, beta), beta)
	where
	pi_r = toInteger $ length beta
-}
