{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Av123 where

import Internal

import qualified Math.Sym as S

type Perm123 = Permutation

instance Catalan Perm123 where
	empty = []
	cons = mkIndec
	decons = decompose


mkIndec :: Perm123 -> Perm123 -> Perm123
mkIndec alpha beta = undefined


decompose :: Perm123 -> Maybe(Perm123, Perm123)
decompose sigma = if S.avoids (permToString sigma) ["123"]
		then Just ([1,2], [3]) --currently to test
		else Nothing
{-----------------------------------------------------------
	Permutation Graphics
------------------------------------------------------------}

draw :: Perm123 -> IO ()
draw perm = drawPerm perm
