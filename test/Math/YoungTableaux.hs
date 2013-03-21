{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

 -- Module      : Math.CatalanStructures.YoungTableaux
-- Copyright   : (c) Stuart Paton 2013
-- License     : undecided
-- Maintainer  : Stuart Paton <stuart.john.paton@gmail.com>
-- 

module Math.YoungTableaux where

import Data.List

import Math.Internal

type Row = [Int]

type Tableau = [Row]

newtype Tableaux = Tableaux {tableaux :: Tableau} deriving (Eq, Ord, Show)

instance Catalan Tableaux where
  empty = Tableaux []
  cons = compose
  decons = decompose

example :: Tableaux
example = Tableaux [[1,3,4,6,7], [2,5,8,10], [9]]

example2 :: Tableaux
example2 = Tableaux [[1,2,3],[4,5,6]]

empty_ :: Tableaux
empty_ = Tableaux []

compose :: Tableaux -> Tableaux -> Tableaux
compose a b = undefined

decompose :: Tableaux -> Maybe (Tableaux, Tableaux)
decompose (Tableaux []) = Nothing
decompose yt = Just . splitT $ getColumnsL yt

splitT :: Tableau -> (Tableaux, Tableaux)
splitT yt = (Tableaux a, Tableaux b)
	where
	(a,b) = if ((length $ Data.List.last yt) == 1)
		then (init' (init' yt), [last' (init' yt) ,last' yt])
		else (init' yt, [last' yt])	
	init' = Data.List.init
	last' = Data.List.last

shape :: Tableau -> [Int]
shape = map length

lambda :: Tableau -> Int
lambda = sum . shape

getRowsL :: Tableaux -> Tableau
getRowsL = tx2t 

getRowsT :: Tableaux -> Tableaux
getRowsT yt = yt

getColumnsL :: Tableaux -> Tableau
getColumnsL = transpose . tx2t

getColumnsT :: Tableaux -> Tableaux
getColumnsT = t2tx . getColumnsL

t2tx :: Tableau -> Tableaux
t2tx yt = Tableaux yt

tx2t :: Tableaux -> Tableau
tx2t (Tableaux yt) = yt
