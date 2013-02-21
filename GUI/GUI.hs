module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

main :: IO ()
main = do
	initGUI --initialise GUI toolkit
	
	builder <- initBuilder

	{----------------------------------------
		Main Window
	-----------------------------------------}
	main_window <- builderGetObject builder castToWindow "main_window"

	mainComboBox <- builderGetObject builder castToComboBox "structureComboBox"

	mQuit <- builderGetObject builder castToMenuItem "quitItem"
	on mQuit menuItemActivate $ runCloseProcedure main_window

	mAbout <- builderGetObject builder castToMenuItem "aboutItem"
	on mAbout menuItemActivate $ showAboutDialog builder

	btnApply <- builderGetObject builder castToButton "applyButton"

	onClicked btnApply $ chooseComboItem builder (getComboIndex mainComboBox)

	btnQuit <- builderGetObject builder castToButton "quitButton"
	onClicked btnQuit $ runCloseProcedure main_window

	{----------------------------------------
		Structure Windows
	-----------------------------------------}


	{----------------------------------------
		Run GUI
	-----------------------------------------}
	widgetShowAll main_window
	mainGUI --run main event loop

initBuilder :: IO Builder
initBuilder = do
	builder <- builderNew -- initialise new builder
	builderAddFromFile builder "mainGui.glade" -- add .glade file to the builder
	return builder

showAboutDialog :: Builder -> IO ()
showAboutDialog builder = do
	about <- builderGetObject builder castToDialog "aboutWindow"
	set about [widgetVisible := True]
	dialogRun about
	set about [widgetVisible := False]

runCloseProcedure :: WidgetClass self => self -> IO ()
runCloseProcedure window = do
	widgetDestroy window
	mainQuit

getComboIndex :: ComboBoxClass self => self -> IO Int
getComboIndex cb = do
	selected <- comboBoxGetActive cb
	return selected

{----------------------------------------------
For combo boxes, each entry is mapped to an integer,
and the mappings are as follows:
0 - Dyck Path
1 - Triangulations of an (n+2)-gon
2 - Young Tableaux
3 - 231-avoiding permutations
4 - 123-avoiding permutations
5 - 213-avoiding permutations
----------------------------------------------}
chooseComboItem :: Builder -> IO Int -> IO ()
chooseComboItem builder n = do
	choice <- n
	case choice of
		0 -> loadDyckPathScreen builder
		1 -> undefined
		2 -> undefined
		3 -> loadStackSortPermScreen builder
		4 -> undefined
		5 -> undefined
		_ -> print choice
	print choice

loadDckPathScreen :: Builder -> IO ()
loadDyckPathScreen builder = undefined

loadStackSortPermScreen :: Builder -> IO ()
loadStackSortPermScreen = undefined

