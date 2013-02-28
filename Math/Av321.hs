module Av321 where

import Internal
import qualified Av123 as P


data Perm321 = Empty | Perm321 Permutation deriving (Eq, Ord, Show)

get321 :: P.Perm123 -> Perm321
get321 p = case p of
	P.Empty -> Empty
	P.Perm p' -> case p' of
		p' -> Perm321 $ revPerm p'

get123 :: Perm321 -> P.Perm123
get123 p = case p of
	Empty -> P.Empty
	Perm321 p' -> case p' of
		p' -> P.Perm $ revPerm p'

