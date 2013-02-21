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

type Partition = [Row]
type Row = [Int]

--data YT = Empty | Partition deriving (Eq, Ord, Show)

type Tableau = [[Int]]
data YT = Empty | Tableau deriving Show


instance Catalan YT where
  empty = Empty
  cons = undefined
  decons = undefined--decompose

example :: Tableau
example = [[1,3,4,6,7], [2,5,8,10], [9]]

emptyExample :: YT
emptyExample = empty

--decompose :: Tableau -> a
decompose = getColumns

shape :: Tableau -> [Int]
shape = map length

lambda :: Tableau -> Int
lambda = sum . shape

getRows :: Tableau -> [[Int]]
getRows yt = yt

getColumns :: Tableau -> [[Int]]
getColumns = transpose
