-- Module      : Math.CatalanStructures
-- Copyright   : (c) Stuart Paton 2013
-- License     : undecided
-- Maintainer  : Stuart Paton <stuart.john.paton@gmail.com>
-- 

module CatalanStructures where

import Internal
import DyckPath
import Av123
import StackSortPerm
import YoungTableaux
import Triangulations

import Control.Monad

import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Vector.Storable as SV



{-----------------------------------------------------------------------------------
	Bijections between structures.
------------------------------------------------------------------------------------}
-- Standard bijection
-- Reference: Classification of Bijections between 321- and 132- avoiding permutations,
--	      Anders Claesson and Sergey Kitaev, 2008
standard :: StackSortablePermutation -> DyckPath
standard [] = []
standard ssp = [U] ++ standard (red (alpha, beta)) ++ [D] ++ standard beta
	where
	(alpha, beta) = stripMaybe $ decons ssp

richards :: DyckPath -> Perm123
richards dp = runST $ do
	v <- MV.unsafeNew n
	foldM_ iter (v, n, Set.empty) [0..n-1]
	SV.unsafeFreeze v
	where
	n = size dp
	iter (v, m, s) i = do
		--TODO:complete 
		let (d,k) = 1 --TODO
		MV.unsafeWrite v i d
		return (v, k, Set.insert d s)
		

--knuthRichards :: StackSortablePermutation -> Perm123
--knuthRichards = standard . richards
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
