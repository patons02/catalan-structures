{-# LANGUAGE NoMonomorphismRestriction #-}
module Test where

import DyckPath
import StackSortPerm
import Internal

exam1 :: DyckPath -> StackSortablePermutation
exam1 = bijection ([U,U,U,D,D,D,U,U,D,U,D,D]::DyckPath)
