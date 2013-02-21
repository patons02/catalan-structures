module PermStat where

import Internal

import Math.Sym.Stat as Stat
{------------------------------------------
	Permutation Statistics
-------------------------------------------}

comp_stat :: Permutation -> Int
comp_stat pi = Stat.comp pi
