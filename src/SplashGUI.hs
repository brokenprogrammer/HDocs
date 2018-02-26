{-# OPTIONS_GHC -fno-warn-unused-do-bind#-}

module SplashGUI where

import Graphics.UI.Gtk hiding (Action, backspace)

data GUI = GUI {
    splashWnd    :: Window,
    splashNew    :: Button,
    splashEdt    :: Button,
    splashSet    :: Button,
    splashQut    :: Button
}

loadSplashGlade :: FilePath -> IO GUI
loadSplashGlade gladePath = do
    builder <- builderNew
    builderAddFromFile builder gladePath

    -- Load Splash Window
    window <- builderGetObject builder castToWindow "SplashWnd"

    [btnNew, btnEdt, btnSet, btnQut] <-
        mapM (builderGetObject builder castToButton)
        ["SplashNew", "SplashEdit", "SplashSettings", "SplashQuit"]

    return $ GUI window btnNew btnEdt btnSet btnQut

connectGUI :: GUI -> IO (ConnectId Button)
connectGUI gui = do
    on (splashWnd gui) objectDestroy mainQuit

    on (splashNew gui) buttonActivated (putStrLn "New")
    on (splashEdt gui) buttonActivated (putStrLn "Edit")
    on (splashSet gui) buttonActivated (putStrLn "Settings")
    on (splashQut gui) buttonActivated mainQuit

main :: FilePath -> IO ()
main gladePath = do
    initGUI

    -- Initialize Splash screen GUI from the glade path.
    gui <- loadSplashGlade gladePath

    -- Connect the GUI to actionlisteners
    connectGUI gui

    widgetShowAll (splashWnd gui)
    mainGUI
