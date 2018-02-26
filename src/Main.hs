module Main where

-- import Control.Monad
-- import Control.Monad.IO.Class
-- import Data.IORef
-- import Graphics.UI.Gtk hiding (Action, backspace)
-- import Graphics.UI.Gtk.Glade
-- import Graphics.UI.Gtk.Builder

import qualified SplashGUI

-- TOOD: 1. New knapp
    -- Steps: 
        -- 1. Användare klickar på New
        -- 2. Message dialog, frågar efter Namn och Template (Template från dropdown)
        -- 3. Användare väljer och klickar okej
        -- 4. Visa main fönstret med laddad tempalte.

main :: IO ()
main = do
    SplashGUI.main "./data/Splash.glade"
    -- initGUI

    -- -- Load glade file
    -- builder <- builderNew
    -- builderAddFromFile builder "./data/Splash.glade"

    -- -- Get a handle to some of the widgets of the glade file.
    -- window <- builderGetObject builder castToWindow "SplashWnd"
    -- btnNew <- builderGetObject builder castToButton "SplashNew"
    -- btnEdt <- builderGetObject builder castToButton "SplashEdit"
    -- btnQut <- builderGetObject builder castToButton "SplashQuit"

    -- -- set window [ windowTitle := "HDocs",
    -- --              windowResizable := False,
    -- --              windowDefaultWidth := 800,
    -- --              windowDefaultHeight := 600 ]

    -- -- Do something with the widgets to test..
    -- on btnNew buttonActivated (putStrLn "NEW ITEM.")
    -- on btnEdt buttonActivated test
    -- on btnQut buttonActivated mainQuit
    -- -- When the window is destroeyd quit the application.
    -- on window objectDestroy mainQuit

    -- widgetShowAll window
    -- mainGUI