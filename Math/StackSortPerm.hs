{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- Module      : Math.CatalanStructures.DyckPath
-- Copyright   : (c) Stuart Paton 2013
-- License     : undecided
-- Maintainer  : Stuart Paton <stuart.john.paton@gmail.com>
-- 

module CatalanStructures.StackSortPerm where

import Internal
import qualified Math.Sym as S

type StackSortablePermutation = Permutation

instance Catalan StackSortablePermutation where
	cons = undefined
	decons gamma = decompose gamma

{-----------------------------------------------------------
	Utility functions
------------------------------------------------------------}

decompose :: StackSortablePermutation -> (StackSortablePermutation, StackSortablePermutation)
decompose gamma = removeHeadSnd $ break (l ==) gamma
	where
	l = toInteger $ length gamma

removeHeadSnd :: (t, [a]) -> (t, [a])
removeHeadSnd (alpha, beta) = (alpha, tail beta)

--listStackSortPerms :: Int -> Permutation
--listStackSortPerms n = filter (isStackSortPerm) (S.perms n :: [Permutation])

displayPerm :: Permutation -> IO()
displayPerm ssp = undefined



