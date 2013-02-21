{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Triangulations where

import Data.List.Split
--import Control.Monad

import Internal

--triangle = (startPt, midPt, endPt)
type Triangle = (Int, Int, Int)

type Triangulations = [Triangle]

instance Catalan Triangulations where
	empty = []	
	cons alpha beta = alpha ++ mkIndec (maximum' $ mapMax alpha) beta
	decons = decompose

{-
instance Functor Triangle where
	fmap = map
-}

example :: Triangulations
example = [(1,4,2), (2,4,3), (5,1,4), (5,1,6), (1,6,8), (8,6,7)]

mkIndec :: Int -> Triangulations -> Triangulations
mkIndec n xs = map (\(x,y,z) -> (x+n, y+n, z+n)) xs

decompose :: Triangulations -> Maybe (Triangulations, Triangulations)
decompose [] = Nothing
decompose xs = Just $ pairDropB1 (splitter xs)

splitter :: Triangulations -> (Triangulations, Triangulations)
splitter xs = splitAt (length xs - lenSnd(splitMappedList xs)) xs

splitMappedList :: Triangulations -> ([Bool], [Bool])
splitMappedList xs = break (True==) (mapList xs)
	where
	ys = xs
	n = length ys 

mapList :: [Triangle] -> [Bool]
mapList [] = []
mapList (x:xs) = ((fstTrip x == 1 || fstTrip x == n) && (trdTrip x == 1 || trdTrip x == n)) : mapList xs
	where
	n = maximum' $ mapMax xs
	
mapMax :: [Triangle] -> [Int]
mapMax = map maxTuple

{-
To decons we need to remove the triangle (1, i, n) and then return the two triangles given.
To cons we need to add the removed triangle to join two triangles THEN renumber.
-}

{------------------------------------------------------------------
	Helper Functions
-------------------------------------------------------------------}
fstTrip, sndTrip, trdTrip :: (a,a,a) -> a
fstTrip (a,b,c) = a
sndTrip (a,b,c) = b
trdTrip (a,b,c) = c

pairDropB1 :: ([a], [a]) -> ([a], [a])
pairDropB1 (a, b) = (a, drop 1 b)

lstToTrip :: [a] -> (a,a,a)
lstToTrip [a,b,c] = (a,b,c) 

maxTuple :: Ord a => (a, a, a) -> a
maxTuple (a,b,c) = maxPair (maxPair (a,b), c)

maxPair :: Ord a => (a, a) -> a
maxPair (a,b) = if a >= b then a else b

maximum' :: (Ord a, Num a) => [a] -> a
maximum' [] = 0
maximum' xs = foldl1 max xs

lenSnd :: ([Bool], [Bool]) -> Int
lenSnd (a,b) = length b
