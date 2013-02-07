{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

 -- Module      : Math.CatalanStructures.YoungTableaux
-- Copyright   : (c) Stuart Paton 2013
-- License     : undecided
-- Maintainer  : Stuart Paton <stuart.john.paton@gmail.com>
-- 

module YoungTableaux where

import Data.List

import Internal

type Tableau = [[Int]]

instance Catalan Tableau where
  empty = [[]]
  cons = undefined
  decons = undefined

example :: Tableau
example = [[1,3,4,6,7],[2,5,8,10],[9]]

shape :: Tableau -> [Int]
shape = map length

lambda :: Tableau -> Int
lambda = sum . shape

getRows :: Tableau -> [[Int]]
getRows yt = yt

getColumns :: Tableau -> [[Int]]
getColumns = transpose