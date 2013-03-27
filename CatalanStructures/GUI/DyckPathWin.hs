module GUI.DyckPath where

import Math.DyckPath

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

main :: IO ()
main = do
	initGUI --initialise GUI toolkit
	
	builder <- initBuilder

	{----------------------------------------
		Main Window
	-----------------------------------------}
	main_window <- builderGetObject builder castToWindow "DyckPath_window"

	mQuit <- builderGetObject builder castToMenuItem "quitItem"
	on mQuit menuItemActivate $ runCloseProcedure main_window

	btnApply <- builderGetObject builder castToButton "btnOkDP"

	entry <- builderGetObject builder castToEntry "txtDyckPath"

	onClicked btnApply $ do
		entry_str <- entryGetText entryText 
		let dp_str = read entry_str
		runDPVisualisation dp_str

	btnQuit <- builderGetObject builder castToButton "quitButton"
	onClicked btnQuit $ runCloseProcedure main_window

initBuilder :: IO Builder
initBuilder = do
	builder <- builderNew -- initialise new builder
	builderAddFromFile builder "dyckPath.glade" -- add .glade file to the builder
	return builder

runCloseProcedure :: WidgetClass self => self -> IO ()
runCloseProcedure window = do
	widgetDestroy window
	mainQuit

getEntryText = do
	entry <- entryGetText buil

runDPVisualisation s = drawDyckPathG dp'
	where
	dp' = str2dp s 

str2dp :: String -> DyckPath
str2dp = map ds
	where
	ds 'U' = U
	ds 'D' = D
