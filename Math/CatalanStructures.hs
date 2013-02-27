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
import qualified Control.Monad.ST as ST

import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Vector.Storable as SV
import qualified Data.IntSet as Set




{-----------------------------------------------------------------------------------
	Bijections between structures.
------------------------------------------------------------------------------------}
-- Standard bijection
-- Reference: Classification of Bijections between 321- and 132- avoiding permutations,
--	      Anders Claesson and Sergey Kitaev, 2008

standard :: StackSortablePermutation -> DyckPath
standard ssp = standard' $ ssptoperm ssp

standard' :: Permutation -> DyckPath
standard' [] = []
standard' ssp = [U] ++ standard' (red (alpha, beta)) ++ [D] ++ standard' beta
	where
	ssp' = permTossp ssp
	(alpha, beta) = remP $ stripMaybe $ decons ssp'
	remP (Perm231 a, Perm231 b) = (a, b)

-- Richards algorithm 
-- Reference: Classification of Bijections between 321- and 132- avoiding permutations,
--	      Anders Claesson and Sergey Kitaev, 2008

{-
richards :: DyckPath -> Perm123
richards dp = permToperm123 $ permvectoperm $ richards' $ dp2pv $ binaryMap dp
	where
	binaryMap dp = dy dp
		where
		dy D = 0
		dy U = 1
	dp2pv = SV.toList

richards' :: PermVec -> PermVec
richards' dp = ST.runST $ do
	v <- MV.unsafeNew n
	foldM_ it (v, m, k) [0..n-1]
	SV.unsafeFreeze v
	where
	n = length dp
	it (v, m, k) i = do
		MV.unsafeWrite v _ _
		return (v, _, _)
-}

-- Simion Schmidt bijection
-- Reference: Classification of Bijections between 321- and 132- avoiding permutations,
--	      Anders Claesson and Sergey Kitaev, 2008

simionSchmidt :: Perm123 -> StackSortablePermutation
simionSchmidt p = permTossp $ permvectoperm $ simionSchmidt' $ permtopermvec $ perm123toperm p

simionSchmidt' :: PermVec -> PermVec
simionSchmidt' p = ST.runST $ do
	v <- MV.unsafeNew n
	foldM_ it (v, n, Set.empty) [0..n-1]
	SV.unsafeFreeze v
	where
	n = SV.length p
	it (v, m, k) i = do
		let c = p SV.! i
		let y = Prelude.head [z | z <- [m+1 ..], z `Set.notMember` k]
		let (d, b) = if c < m then (c,c) else (y, m)
		MV.unsafeWrite v i d
		return (v, b, Set.insert d k)

simionSchmidtInv :: StackSortablePermutation -> Perm123
simionSchmidtInv p = permToperm123 $ permvectoperm $ simionSchmidtInv' $ permtopermvec $ ssptoperm p

simionSchmidtInv' :: PermVec -> PermVec
simionSchmidtInv' p = ST.runST $ do
	v <- MV.unsafeNew n
	let iter = [0..n-1]
	foldM_ it (v, n, Set.fromAscList iter) iter
	SV.unsafeFreeze v
	where
	n = SV.length p
	it (v, m, k) i = do
		let c = p SV.! i
		let (d, b) = if c < m then (c,c) else (Set.findMax k, m)
		MV.unsafeWrite v i d
		return (v, b, Set.delete d k)


--Knuth-Richards bijection
-- Reference: Classification of Bijections between 321- and 132- avoiding permutations,
--	      Anders Claesson and Sergey Kitaev, 2008


{-----------------------------------------------------------------------------------
	Helper functions.
------------------------------------------------------------------------------------}
stripMaybe :: Maybe (a, a) -> (a, a)	
stripMaybe (Just xs) = xs

--reducation function
red :: (Permutation, Permutation) -> Permutation
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
