{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- Module      : Math.CatalanStructures
-- Copyright   : (c) Stuart Paton 2013
-- License     : undecided
-- Maintainer  : Stuart Paton <stuart.john.paton@gmail.com>
-- 

module CatalanStructures where

import qualified Math.Sym as S 
import qualified Data.Text as T

--import CatalanStructures.DyckPath
--import CatalanStructures.StackSortPerm

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

{-
instance Catalan DyckPath where
	cons alpha beta = ['a'] ++ alpha ++ ['d'] ++ beta
	decons gamma    = undefined
-}

{-
instance Catalan StackSortablePermutation where
	cons = undefined
	decons = undefined
-}

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

--Todo: tidy this into appropriate function!
isDyckPath :: DyckPath -> Bool
isDyckPath path = not (isDyckPath' path)

isDyckPath' :: DyckPath -> Bool
isDyckPath' path
	| head path /= 'u' = False
	| (T.last $ T.pack path) /= 'd' = False
	| (length path `mod` 2 == 0) && (countsEqual u_cnt d_cnt path) = False
	| otherwise = True

{-----------------------------------------------------------------------------------
	Helper functions.
------------------------------------------------------------------------------------}

--Counts the amount of given elements in a list
count :: Eq a => a -> [a] -> Int
count x ys = length (filter (== x) ys) 

--takes u count
u_cnt :: [Char] -> Int
u_cnt xs = count 'u' xs

--takes d count
d_cnt :: [Char] -> Int
d_cnt xs = count 'd' xs

--checks that the counts are equal
countsEqual :: Eq a => (t -> a) -> (t -> a) -> t -> Bool
countsEqual f f' xs = f xs == f' xs

