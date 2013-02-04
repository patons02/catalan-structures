{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- Module      : Math.CatalanStructures.DyckPath
-- Copyright   : (c) Stuart Paton 2013
-- License     : undecided
-- Maintainer  : Stuart Paton <stuart.john.paton@gmail.com>
-- 

module CatalanStructures.DyckPath where

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
