{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- Module      : Math.CatalanStructures.DyckPath
-- Copyright   : (c) Stuart Paton 2013
-- License     : undecided
-- Maintainer  : Stuart Paton <stuart.john.paton@gmail.com>
-- 

module Math.DyckPath where

import Math.Internal

import Control.Monad.Trans(liftIO)
import Data.List
import Data.List.Split

import Diagrams.Prelude
import Diagrams.Backend.Cairo.Internal
import Diagrams.Backend.Cairo.Gtk
import Graphics.UI.Gtk
import Graphics.Rendering.Diagrams.Core

--import Graphics.Gloss

data Step = U | D deriving (Eq, Show)

type DyckPath = [Step]

type Point = (Integer, Integer)

instance Catalan DyckPath where
	empty = []
	cons alpha beta = mkIndec alpha ++ beta 
	decons = decompose

{------------------------------------------------------------------
	Make indecomposable and decomposable Dyck Paths
-------------------------------------------------------------------}

mkIndec :: DyckPath -> DyckPath
mkIndec alpha = [U] ++ alpha ++ [D]

heights :: DyckPath -> [Int]
heights = scanl (+) 0 . map dy
	where 
	dy U = 1
	dy D = -1


-- O(n^2) version!
--heights :: DyckPath -> [Int]
--heights = map sum . inits .map dy
--         where
--           dy U = 1
--           dy D = -1

decompose :: DyckPath -> Maybe (DyckPath, DyckPath)
decompose [] = Nothing
decompose xs@(U:xt) = Just (map fst (init ys), map fst zs) 
               where
                 0:ht = heights xs 
                 (ys, zs) = span(\(_, h) -> h > 0) $ zip xt ht

--dyckPath2Points :: DyckPath -> [Point]
--dyckPath2Points (x:xs) = undefined

{------------------------------------------------------------------
	Statistics
-------------------------------------------------------------------}
dpstats :: [DyckPath -> Int]
dpstats = [uCnt, dCnt, returnsXAxis, peaks, heightStat, noInitialRises]

--Number of up steps
--added 06/02/2013
uCnt :: DyckPath -> Int
uCnt = count U 

--Number of down steps
--added 06/02/2013
dCnt :: DyckPath -> Int
dCnt = count D 

--Number of returns to the x axis
--added 06/02/2013
returnsXAxis :: DyckPath -> Int
returnsXAxis dp = (count 0 $ heights dp) - 1

--Number of peaks
{- algorithm:
1) split into lists at each 0
2) find number of highest element of each list
3) sum of counts from step 2
-} 
--added 06/02/2013
peaks :: DyckPath -> Int
peaks dp = sum $ largestElemCnt $ split
	where
	split = splitWhen (== 0) $ heights dp

--added 07/02/2013
heightStat :: DyckPath -> Int 
heightStat dp = maximum $ heights dp

--foreign import ccall unsafe "dyckPathStat.h init" c_init :: Ptr CLong -> CLong -> CLong


--added 07/02/2013      background = square (fromIntegral n) # fc whitesmoke # lc white
{-
The number of consecutive up steps
uses FFI
-}
noInitialRises :: DyckPath -> Int
noInitialRises = undefined
--noInitialRises dp = c_init

{------------------------------------------------------------------
	Helper functions
-------------------------------------------------------------------}
count :: Eq a => a -> [a] -> Int
count x ys = length (filter (== x) ys)

largestElemCnt :: Ord a => [[a]] -> [Int]
largestElemCnt [[]] = [0]
largestElemCnt (xs:xss) = count (maximum xs) xs : largestElemCnt xss 

toString :: DyckPath -> String
toString = map dy
	where
	dy U = 'u'
	dy D = 'd'

fromString :: String -> DyckPath
fromString = map dy
	where
	dy 'u' = U
	dy 'd' = D

int2floatlst :: [Int] -> [Float]
int2floatlst = map (fromIntegral)

int2float :: Int -> Float
int2float = fromIntegral
{------------------------------------------------------------------
	Graphics
-------------------------------------------------------------------}
example = [U,D,U,U,D,D,U,D,U,D]
ex2 = [U,D,U,D]

mappy :: DyckPath -> [(Float, Float)]--Graphics.Gloss.Path --[(Float, Float)]--Path
mappy = map dy
	where
	dy U = (1,1)
	dy D = (1,-1)

extract2nd :: [(a,b)] -> [b]
extract2nd  = map (\(x,y) -> y) 

buildCoords :: DyckPath -> [(Float, Float)]--Graphics.Gloss.Path --[(Float, Float)]--Path
buildCoords xs = [(0.0,0.0)] ++ zip (take len [1.0..]) (extract2nd $ mappy xs)
	where
	len = length $ heights xs

--using gloss
--drawDyckPathG :: DyckPath -> IO ()
--drawDyckPathG dp = display (InWindow "Dyck Path" (300,300) (300,300)) Graphics.Gloss.white (Graphics.Gloss.scale 40 20 $ Line $ buildCoords dp)

{-
--using diagrams
drawDyckPathDia :: DyckPath -> IO ()
drawDyckPathDia dp = do
	initGUI
	window <- windowNew
	canvas <- drawingAreaNew
	canvas `on` sizeRequest $ return (Requisition 256 256)
	set window [windowTitle := "Dyck Path", containerBorderWidth := 10, containerChild := canvas ]
	canvas `on` exposeEvent $ renderFigure canvas dp
	onDestroy window mainQuit
	widgetShowAll window
	mainGUI
	
renderFigure :: DrawingArea -> DyckPath -> EventM EExpose Bool
renderFigure canvas dp = do
	liftIO $ defaultRender canvas $ figure2Render dp
	return True

figure2Render :: DyckPath -> DC 
figure2Render dp = plotPath dp

plotPath :: DyckPath -> DC
plotPath dp = centerXY `atop` background --alignX (-1) `atop` background
	where
	n = length dp
	line = repeat $ line # lc black
	background = square (fromIntegral n) # lc black # fc white
-}

