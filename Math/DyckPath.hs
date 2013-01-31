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
	cons alpha beta = mkIndec alpha ++ beta :: DyckPath 
	decons = undefined --help?
{------------------------------------------------------------------
	Make indecomposable and decomposable Dyck Paths
-------------------------------------------------------------------}
mkIndec :: DyckPath -> DyckPath
mkIndec alpha = ['u'] ++ alpha ++ ['d']

mkDecom :: DyckPath -> [DyckPath]
mkDecom xs = concat [[fst (decomp xs)], snd (decomp xs)]

decomp :: DyckPath -> (DyckPath, [DyckPath])
decomp xs = (extractFirstandLast xs, rest xs)

decompose :: DyckPath -> [DyckPath]
decompose gamma = stripMaybe $ deconsUtil gamma

deconsUtil :: DyckPath -> Maybe [DyckPath]
deconsUtil gamma = if isDyckPath gamma then Just (gammaList gamma) else Nothing

{-
gammaList decomposes the dyck path into a list of smaller dyck paths!
It takes a chunk of gamma off at a time then append it to a list
-}
gammaList :: DyckPath -> [DyckPath]
gammaList gamma = do --todo REPLACE WITH ACTUAL FUNCTION!
	g <- gamma
	
	return gamma'	

		




gammaList' (g:gs) n m
	| n < 0 = undefined
	| gammaList' gs (n+1) (m+1) = undefined
	| gammaList' gs (n-1) (m+1) = undefined


--testy gamma = mkIndec . decompose . (perms gamma)
{------------------------------------------------------
	Helper functions
-------------------------------------------------------}

stripMaybe :: Maybe [a] -> [a]
stripMaybe (Just xs) = xs
stripMaybe Nothing = []

extractFirstandLast :: [a] -> [a]
extractFirstandLast xs = [head xs, last xs]

stripFirstandLast = stripLast . stripFirst

stripFirst :: [a] -> [a]
stripFirst (p:ps) = ps

stripLast :: [a] -> [a]
stripLast xs = take (length xs -1) xs 

some :: DyckPath -> DyckPath
some (x:xs) = xs

rest :: Monad m => DyckPath -> m DyckPath
rest xs = do
	let t = some xs
	let u = init t
	return u


