module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

main :: IO ()
main = do
	initGUI --initialise GUI toolkit
	
	builder <- initBuilder

	main_window <- builderGetObject builder castToWindow "main_window"

	mQuit <- builderGetObject builder castToMenuItem "quitItem"
	on mQuit menuItemActivate $ runCloseProcedure main_window

	mAbout <- builderGetObject builder castToMenuItem "aboutItem"
	on mAbout menuItemActivate $ showAboutDialog builder

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
