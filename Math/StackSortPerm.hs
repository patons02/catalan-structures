{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- Module      : Math.CatalanStructures.DyckPath
-- Copyright   : (c) Stuart Paton 2013
-- License     : undecided
-- Maintainer  : Stuart Paton <stuart.john.paton@gmail.com>
-- 

module StackSortPerm where

import Control.Monad.Trans(liftIO)
import Data.Char 

import Diagrams.Prelude
import Diagrams.Backend.Cairo.Internal
import Diagrams.Backend.Cairo.Gtk
import Graphics.UI.Gtk
import Graphics.Rendering.Diagrams.Core

import qualified Math.Sym as S
import qualified Math.Sym.Plot as P 

import Internal

type DC = Diagram Cairo R2

type StackSortablePermutation = Permutation

instance Catalan StackSortablePermutation where
	empty = []
	cons = mkIndec
	decons = decompose

{-----------------------------------------------------------
	Utility functions
------------------------------------------------------------}

mkIndec :: StackSortablePermutation -> StackSortablePermutation -> StackSortablePermutation
mkIndec alpha beta = alpha ++ [n] ++ beta
	where
	n = length (alpha ++ beta) + 1

decompose :: StackSortablePermutation -> Maybe (StackSortablePermutation, StackSortablePermutation)
decompose sigma = if S.avoids (permToString sigma) ["231"]
		  then Just (removeHeadSnd $ break (l ==) sigma)
		  else Nothing
	where
	l = length sigma

removeHeadSnd :: (t, [a]) -> (t, [a])
removeHeadSnd (alpha, beta) = (alpha, tail beta)

(<+>) :: Permutation -> Permutation -> Permutation
pi <+> rho =  pi ++ rho' rho
	where
	rho' [] = []
	rho' (x:xs) = length pi + x : rho' xs 

alpha :: Permutation -> StackSortablePermutation
alpha pi = n : pi
	where
	n = length pi + 1

--In O(n) time complexity!
permToString :: Permutation -> String
permToString xs = foldr ((++) . show) "" xs

--below is the inefficient version of permToString in O(n^2) time complexity!
--permToString [] = ""
--permToString (x:xs) = show x ++ permToString xs

stringToPerm :: String -> Permutation
stringToPerm s = xs --map toInteger xs
	where
	xs = map digitToInt s

{-----------------------------------------------------------
	Graphics functions
------------------------------------------------------------}

drawPerm :: StackSortablePermutation -> IO ()
drawPerm perm = do
	initGUI
	window <- windowNew
	canvas <- drawingAreaNew
	canvas `on` sizeRequest $ return (Requisition 256 256)
	set window [windowTitle := "Permutation Matrix", containerBorderWidth := 10, containerChild := canvas ]
	canvas `on` exposeEvent $ renderFigure canvas perm
	onDestroy window mainQuit
	widgetShowAll window
	mainGUI

renderFigure :: DrawingArea -> StackSortablePermutation -> EventM EExpose Bool
renderFigure canvas perm = do
	liftIO $ defaultRender canvas $ figure2Render perm
	return True

figure2Render :: StackSortablePermutation -> DC 
figure2Render perm = P.plotPerm $ permToString perm

