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
import qualified Math.Sym.Internal as I

type Permutation = [Int]

type PermVec = SV.Vector Int

type DC = Diagram Cairo R2

class Catalan a where
	empty :: a
	cons :: a -> a -> a
	decons :: a -> Maybe (a, a)

bijection :: (Catalan a, Catalan b) => a -> b
bijection w = case decons w of
		Nothing -> empty
		Just (u,v) -> cons (bijection u) (bijection v)

permvectoperm :: PermVec -> Permutation
permvectoperm = SV.toList

permtopermvec :: Permutation -> PermVec
permtopermvec = SV.fromList

isInc :: Permutation -> Bool
isInc p = not (elem (isInc' p))
	where
	isInc' [] = [True]
	isInc' (x:[]) = [True]
	isInc' (x:y:xs) = (y > x) : isInc' xs

isDec :: Permutation -> Bool
isDec p = not (elem False (isDec' p))
	where
	isDec' [] = [True]
	isDec' (x:[]) = [True]
	isDec' (x:y:xs) = (x > y) : isDec' xs 

{-----------------------------------------------------------
	Permutation Operations
------------------------------------------------------------}
compVec :: PermVec -> PermVec
compVec p0 = permtopermvec $ complement $ permvectoperm p0
	
complement :: Permutation -> Permutation
complement p = map (\x -> length p + 1 - x) p 

revPerm :: Permutation -> Permutation
revPerm p = reverse p

{-----------------------------------------------------------
	Permutation Statistics
------------------------------------------------------------}

asc :: Permutation -> Int
asc =  I.asc . permtopermvec

des :: Permutation -> Int
des =  I.des . permtopermvec

exc :: Permutation -> Int
exc = I.exc . permtopermvec

ldr :: Permutation -> Int
ldr = I.ldr . permtopermvec

rdr :: Permutation -> Int
rdr = I.rdr . permtopermvec

lir :: Permutation -> Int
lir =   I.lir . permtopermvec

rir :: Permutation -> Int
rir =   I.rir . permtopermvec

zeil :: Permutation -> Int
zeil =   I.rdr . I.inverse . permtopermvec --zeil = rdr . i 

comp :: Permutation -> Int
comp =   I.comp . permtopermvec

lmax :: Permutation -> Int
lmax =   I.lmax . permtopermvec

lmin :: Permutation -> Int
lmin =   I.lmin . permtopermvec

rmax :: Permutation -> Int
rmax =   I.rmax . permtopermvec

rmin :: Permutation -> Int
rmin =   I.rmin . permtopermvec

head :: Permutation -> Int
head =   I.head . permtopermvec

last :: Permutation -> Int
last =   I.last . permtopermvec

peak :: Permutation -> Int
peak =   I.peak . permtopermvec

valley :: Permutation -> Int
valley =   I.vall . permtopermvec

--lds :: Permutation -> Int
--lds = permtopermvec I.lds

--lis :: Permutation -> Int
--lis = permtopermvec I.lis

rank :: Permutation -> Int
rank =   I.ep . permtopermvec

cyc :: Permutation -> Int
cyc =   I.cyc . permtopermvec

--fix :: Permutation -> Int
--fix = permtopermvec I.fix

--slmax :: Permutation -> Int
--slmax = permtopermvec I.slmax

{-----------------------------------------------------------
	Permutation Graphics
------------------------------------------------------------}

drawPerm :: Permutation -> IO ()
drawPerm perm = do
	initGUI
	window <- windowNew
	canvas <- drawingAreaNew
	canvas `on` sizeRequest $ return (Requisition 256 256)
	set window [windowTitle := "Permutation Int Matrix", containerBorderWidth := 10, containerChild := canvas ]
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
