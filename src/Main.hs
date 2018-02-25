module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)

main :: IO ()
main = do
    void initGUI
    window <- windowNew

    set window [ windowTitle := "HDocs",
                 windowResizable := False,
                 windowDefaultWidth := 800,
                 windowDefaultHeight := 600 ]

    -- When the window is destroyed quit the application.
    on window objectDestroy mainQuit

    widgetShowAll window
    mainGUI