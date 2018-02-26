module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Builder

test:: IO ()
test = putStrLn "EDIT"

-- TODO: 2. Quit knapp 
-- TOOD: 3. New knapp - Öppnar main fönstret ?? 

main :: IO ()
main = do
    initGUI

    -- Load glade file
    builder <- builderNew
    builderAddFromFile builder "./data/Splash.glade"

    -- Get a handle to some of the widgets of the glade file.
    window <- builderGetObject builder castToWindow "SplashWnd"
    btnNew <- builderGetObject builder castToButton "SplashNew"
    btnEdt <- builderGetObject builder castToButton "SplashEdit"

    -- set window [ windowTitle := "HDocs",
    --              windowResizable := False,
    --              windowDefaultWidth := 800,
    --              windowDefaultHeight := 600 ]

    -- Do something with the widgets to test..
    on btnNew buttonActivated (putStrLn "NEW ITEM.")
    on btnEdt buttonActivated test
    -- When the window is destroeyd quit the application.
    on window objectDestroy mainQuit

    widgetShowAll window
    mainGUI