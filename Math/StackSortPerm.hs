{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- Module      : Math.CatalanStructures.DyckPath
-- Copyright   : (c) Stuart Paton 2013
-- License     : undecided
-- Maintainer  : Stuart Paton <stuart.john.paton@gmail.com>
-- 

module CatalanStructures.StackSortPerm where

import CatalanStructures
import qualified Math.Sym as S

instance Catalan StackSortablePermutation where
	cons = undefined
	decons = undefined

mkStackSortPerm :: Int -> [a]
mkStackSortPerm n = (S.perms n)



