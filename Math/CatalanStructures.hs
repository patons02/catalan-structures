-- Module      : Math.CatalanStructures
-- Copyright   : (c) Stuart Paton 2013
-- License     : undecided
-- Maintainer  : Stuart Paton <stuart.john.paton@gmail.com>
-- 

module CatalanStructures (isStackSortPerm, isDyckPath) where

import qualified Math.Sym as S 
import qualified Data.Text as T

-- A Dyck path is an encoding where u is an up-step and d is a down-step
type Permutation = String

type DyckPath = [Char]
type StackSortablePermutation = Permutation

class Catalan a where
	cons :: a -> a -> a
	decons :: a -> (a, a)

{-----------------------------------------------------------------------------------
	Create instances of each Catalan structure.
------------------------------------------------------------------------------------}
instance Catalan DyckPath where
	cons = undefined
	decons = undefined
	
instance Catalan StackSortablePermutation where
	cons = undefined
	decons = undefined

{-----------------------------------------------------------------------------------
	Tests that structures are structures.
------------------------------------------------------------------------------------}

{----------------------
Tests if the given permutation is a 132- and 231-avoiding permutation.
Known as a one-stack sortable permutation
-----------------------}
isStackSortPerm :: Permutation -> Bool
isStackSortPerm sigma = sigma `S.avoids` ["132", "231"] 

{----------------------
Tests if the given DyckPath is actually a DyckPath and not just a [Char].
Has to satisfy the conditions:
Start with an up-step, never go below the x-axis, and must end on the x-axis.
That is, must start with a "u", number of "u"'s must equal number of "d"'s and must end with a "d"
-----------------------}
isDyckPath :: DyckPath -> Bool
isDyckPath path
	|head path != "u" = False
	|T.last $ T.pack path != "d" = False
	|‬(length path % 2) && (u_cnt == d_cnt) = False
	|otherwise = True
	where
		u_cnt = count "u" path
		d_cnt = count "d" path

{-----------------------------------------------------------------------------------
	Helper functions.
------------------------------------------------------------------------------------}

--Counts the amount of given elements in a list
count :: Eq a => a -> [a] -> Int
count x ys = length (filter (== x) ys) 
