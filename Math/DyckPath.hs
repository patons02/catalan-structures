{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- Module      : Math.CatalanStructures.DyckPath
-- Copyright   : (c) Stuart Paton 2013
-- License     : undecided
-- Maintainer  : Stuart Paton <stuart.john.paton@gmail.com>
-- 

module DyckPath where

import Internal
import Data.List

data Step = U | D deriving (Eq, Show)

type DyckPath = [Step]

type Point = (Integer, Integer)

instance Catalan DyckPath where
	empty = []
	cons alpha beta = mkIndec alpha ++ beta 
	decons = decompose

{------------------------------------------------------------------
	Make indecomposable and decomposable Dyck Paths
-------------------------------------------------------------------}

mkIndec :: DyckPath -> DyckPath
mkIndec alpha = [U] ++ alpha ++ [D]

height :: DyckPath -> [Int]
height = scanl (+) 0 . map dy
	where 
	dy U = 1
	dy D = -1

-- O(n^2) version!
--height :: DyckPath -> [Int]
--height = map sum . inits .map dy
--         where
--           dy U = 1
--           dy D = -1

decompose :: DyckPath -> Maybe (DyckPath, DyckPath)
decompose [] = Nothing
decompose xs@(U:xt) = Just (map fst (init ys), map fst zs) 
               where
                 0:ht = height xs 
                 (ys, zs) = span(\(_, h) -> h > 0) $ zip xt ht

dyckPath2Points :: DyckPath -> [Point]
dyckPath2Points (x:xs) = undefined

{------------------------------------------------------------------
	Statistics
-------------------------------------------------------------------}
uCnt :: DyckPath -> Int
uCnt = count U 

dCnt :: DyckPath -> Int
dCnt = count D 

returnsXAxis :: DyckPath -> Int
returnsXAxis dp = count 0 $ height dp

peaks :: DyckPath -> Int
peaks dp = count 2 $ height dp

globalMax :: DyckPath -> Int
globalMax = undefined

globalMin :: DyckPath -> Int
globalMin = undefined

localMax :: DyckPath -> Int
localMax = undefined

localMin :: DyckPath -> Int
localMin = undefined

{------------------------------------------------------------------
	Helper functions
-------------------------------------------------------------------}
count :: Eq a => a -> [a] -> Int
count x ys = length (filter (== x) ys) 


