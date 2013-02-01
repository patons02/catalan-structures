{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- Module      : Math.CatalanStructures.DyckPath
-- Copyright   : (c) Stuart Paton 2013
-- License     : undecided
-- Maintainer  : Stuart Paton <stuart.john.paton@gmail.com>
-- 

module CatalanStructures.StackSortPerm where

import Internal
import qualified Math.Sym as S

type StackSortablePermutation = Permutation
type Permutation = [Integer]

instance Catalan StackSortablePermutation where
	cons = undefined
	decons = undefined

{-----------------------------------------------------------
	Utility functions
------------------------------------------------------------}


highestElemPosition :: Permutation -> Maybe (Permutation, Permutation)
highestElemPostion [] = Nothing
highestElemPosition xs = Just $ 

--listStackSortPerms :: Int -> Permutation
--listStackSortPerms n = filter (isStackSortPerm) (S.perms n :: [Permutation])

displayPerm :: Permutation -> IO()
displayPerm ssp = undefined



