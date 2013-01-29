{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- Module      : Math.CatalanStructures.DyckPath
-- Copyright   : (c) Stuart Paton 2013
-- License     : undecided
-- Maintainer  : Stuart Paton <stuart.john.paton@gmail.com>
-- 

module CatalanStructures.DyckPath where

import CatalanStructures 

instance Catalan DyckPath where
	cons alpha beta = mkIndec alpha ++ beta 
	decons = undefined

{------------------------------------------------------
	
-------------------------------------------------------}
mkIndec :: DyckPath -> DyckPath
mkIndec alpha = ['u'] ++ alpha ++ ['d']

mkDecom :: DyckPath -> [DyckPath]
mkDecom xs = concat [[fst (decompose xs)], snd (decompose xs)]

decompose :: DyckPath -> (DyckPath, [DyckPath])
decompose xs = (extractFirstandLast xs, rest xs)

{------------------------------------------------------
	Helper functions
-------------------------------------------------------}

extractFirstandLast :: [a] -> [a]
extractFirstandLast xs = [head xs, last xs]

some :: DyckPath -> DyckPath
some (x:xs) = xs

rest xs = do
	let t = some xs
	let u = init t
	return u
