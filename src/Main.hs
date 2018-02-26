module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Builder

main :: IO ()
main = do
    initGUI

    -- Load glade file
    -- splashXMLM <- xmlNew "Splash.glade"
    -- let splashXML = case splashXMLM of
    --         (Just splashXML) -> splashXML
    --         Nothing          -> error "Cannong find the glade file \"Splash.glade\"."

    builder <- builderNew
    builderAddFromFile builder "./data/Splash.glade"

    -- Get a handle to some of the widgets of the glade file.
    -- window <- xmlGetWidget splashXML castToWindow "SplashWnd"
    -- btnNew <- xmlGetWidget splashXML castToButton "SplashNew"
    window <- builderGetObject builder castToWindow "SplashWnd"
    btnNew <- builderGetObject builder castToButton "SplashNew"


    -- Do something with the widgets to test..
    --btnNew `onClicked` putStrLn "New Item.."
   -- window `onDestroy` mainQuit
    on btnNew buttonActivated (putStrLn "NEW ITEM.")
    on window objectDestroy mainQuit
    -- window <- windowNew

    -- set window [ windowTitle := "HDocs",
    --              windowResizable := False,
    --              windowDefaultWidth := 800,
    --              windowDefaultHeight := 600 ]

    -- -- When the window is destroyed quit the application.
    -- on window objectDestroy mainQuit

    widgetShowAll window
    mainGUI