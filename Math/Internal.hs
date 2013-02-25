module Internal where

import Control.Monad.Trans(liftIO)
import Data.Char 
import qualified Data.Vector.Storable as SV

import Diagrams.Prelude
import Diagrams.Backend.Cairo.Internal
import Diagrams.Backend.Cairo.Gtk
import Graphics.UI.Gtk
import Graphics.Rendering.Diagrams.Core

import qualified Math.Sym.Plot as P 

type Permutation = [Int]

type Perm0 = SV.Vector Int

type DC = Diagram Cairo R2

class Catalan a where
	empty :: a
	cons :: a -> a -> a
	decons :: a -> Maybe (a, a)

bijection :: (Catalan a, Catalan b) => a -> b
bijection w = case decons w of
		Nothing -> empty
		Just (u,v) -> cons (bijection u) (bijection v)

perm0toperm :: Perm0 -> Permutation
perm0toperm = SV.toList

permtoperm0 :: Permutation -> Perm0
permtoperm0 = SV.fromList

{-----------------------------------------------------------
	Permutation Graphics
------------------------------------------------------------}

drawPerm :: Permutation -> IO ()
drawPerm perm = do
	initGUI
	window <- windowNew
	canvas <- drawingAreaNew
	canvas `on` sizeRequest $ return (Requisition 256 256)
	set window [windowTitle := "Permutation Matrix", containerBorderWidth := 10, containerChild := canvas ]
	canvas `on` exposeEvent $ renderFigurePerm canvas perm
	onDestroy window mainQuit
	widgetShowAll window
	mainGUI

renderFigurePerm :: DrawingArea -> Permutation -> EventM EExpose Bool
renderFigurePerm canvas perm = do
	liftIO $ defaultRender canvas $ figure2RenderPerm perm
	return True

figure2RenderPerm :: Permutation -> DC 
figure2RenderPerm perm = P.plotPerm $ permToString perm

stringToPerm :: String -> Permutation
stringToPerm s = xs --map toInteger xs
	where
	xs = map digitToInt s

--In O(n) time complexity!
permToString :: Permutation -> String
permToString xs = foldr ((++) . show) "" xs

--below is the inefficient version of permToString in O(n^2) time complexity!
--permToString [] = ""
--permToString (x:xs) = show x ++ permToString xs
