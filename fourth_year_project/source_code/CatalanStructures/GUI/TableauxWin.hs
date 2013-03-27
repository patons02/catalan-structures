module GUI.Tableaux where

import Math.YoungTableaux as YT

main :: IO ()
main = do
	initGUI --initialise GUI toolkit
	
	builder <- initBuilder

	{----------------------------------------
		Main Window
	-----------------------------------------}
	main_window <- builderGetObject builder castToWindow "Tableaux_window"

	btnApply <- builderGetObject builder castToButton "btnOkTab"

	entry <- builderGetObject builder castToEntry "txtTab"

	onClicked btnApply $ do
		entry_str <- entryGetText entry
		let yt_str = read entry_str
		runPermVisualisation yt_str

	btnQuit <- builderGetObject builder castToButton "quitButton"
	onClicked btnQuit $ runCloseProcedure main_window

initBuilder :: IO Builder
initBuilder = do
	builder <- builderNew -- initialise new builder
	builderAddFromFile builder "Tableaux.glade" -- add .glade file to the builder
	return builder

runCloseProcedure :: WidgetClass self => self -> IO ()
runCloseProcedure window = do
	widgetDestroy window
	mainQuit

runTabVisualisation yt_str = drawYTG yt
	where
	yt = tab2tabx (ls yt_str :: Tableau)

tab2tabx yt = Tableaux yt

ls :: [String] -> [[Int]]
ls [] = []
ls (x:xs) = (intsplit $ str2int' x) : ls xs

intsplit :: [Int] -> [Int]
intsplit i = map (read) [[z] | z <- [x | k <- (map (show) i), x <- k]]

wordsP     :: (Char -> Bool) -> String -> [String]
wordsP pred s =  case dropWhile pred s of
                      "" -> []
                      s' -> w : wordsWhen pred s''
                            where (w, s'') = break pred s'

