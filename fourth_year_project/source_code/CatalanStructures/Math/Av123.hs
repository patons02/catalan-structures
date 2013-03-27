{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- Module      : Math.Av123
-- Copyright   : (c) Stuart Paton 2013
-- License     : BSD3
-- Maintainer  : Stuart Paton <stuart.john.paton@gmail.com>
-- 

module Math.Av123 where

import Math.Internal

import qualified Math.Sym as S

data Perm123 = Empty | Perm Permutation deriving Show

--type Perm123 = Permutation

instance Catalan Perm123 where
	empty = Empty
	cons = mkIndec
	decons = decompose

mkIndec :: Perm123 -> Perm123 -> Perm123
mkIndec alpha beta = undefined

decompose :: Perm123 -> Maybe(Perm123, Perm123)
decompose sigma = if S.avoids (perm123ToString sigma) ["123"]
		then Just (permToperm123 [1,2], permToperm123 [3]) --currently to test
		else Nothing

{-----------------------------------------------------------
	Utility Functions
------------------------------------------------------------}

perm123ToString :: Perm123 -> String
perm123ToString p = permToString $ perm123toperm p

permToperm123 :: Permutation -> Perm123
permToperm123 p = case p of
	[] -> Empty
	a -> Perm a

perm123toperm :: Perm123 -> Permutation
perm123toperm p = case p of
	Empty -> []
	Perm a -> a

{-----------------------------------------------------------
	Permutation Graphics
------------------------------------------------------------}

draw :: Perm123 -> IO ()
draw perm = drawPerm $ perm123toperm perm
