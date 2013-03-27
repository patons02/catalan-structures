module GUI.Permutation where

import Math.Internal as Internal

main :: IO ()
main = do
	initGUI --initialise GUI toolkit
	
	builder <- initBuilder

	{----------------------------------------
		Main Window
	-----------------------------------------}
	main_window <- builderGetObject builder castToWindow "Permutation_window"

	btnApply <- builderGetObject builder castToButton "btnOkPerm"

	entry <- builderGetObject builder castToEntry "txtPermutation"

	onClicked btnApply $ do
		entry_str <- entryGetText entry
		let perm_str = read entry_str
		runPermVisualisation perm_str

	btnQuit <- builderGetObject builder castToButton "quitButton"
	onClicked btnQuit $ runCloseProcedure main_window

initBuilder :: IO Builder
initBuilder = do
	builder <- builderNew -- initialise new builder
	builderAddFromFile builder "permutation.glade" -- add .glade file to the builder
	return builder

runCloseProcedure :: WidgetClass self => self -> IO ()
runCloseProcedure window = do
	widgetDestroy window
	mainQuit

runPermVisualisation s = Internal.drawPerm ds 
	where	
	ds = str2int :: Internal.Permutaton

str2int :: String -> [Int]
str2int s = intsplit $ str2int' s

str2int' :: String -> [Int]
str2int' s = [read s]

intsplit :: [Int] -> [Int]
intsplit i = map (read) [[z] | z <- [x | k <- (map (show) i), x <- k]]
