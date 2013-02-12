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
import Data.List.Split

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
--Number of up steps
--added 06/02/2013
uCnt :: DyckPath -> Int
uCnt = count U 

--Number of down steps
--added 06/02/2013
dCnt :: DyckPath -> Int
dCnt = count D 

--Number of returns to the x axis
--added 06/02/2013
returnsXAxis :: DyckPath -> Int
returnsXAxis dp = (count 0 $ height dp) - 1

--Number of peaks
{- algorithm:
1) split into lists at each 0
2) find number of highest element of each list
3) sum of counts from step 2
-} 
--added 06/02/2013
peaks :: DyckPath -> Int
peaks dp = sum $ largestElemCnt $ split
	where
	split = splitWhen (== 0) $ height dp

--added 07/02/2013
heightStat :: DyckPath -> Int 
heightStat dp = maximum $ height dp

--added 07/02/2013
noInitialRises :: DyckPath -> Int
noInitialRises dp = undefined
{------------------------------------------------------------------
	Helper functions
-------------------------------------------------------------------}
count :: Eq a => a -> [a] -> Int
count x ys = length (filter (== x) ys)

largestElemCnt :: Ord a => [[a]] -> [Int]
largestElemCnt [[]] = [0]
largestElemCnt (xs:xss) = count (maximum xs) xs : largestElemCnt xss 
