module HDocsGUI where

import Graphics.UI.Gtk hiding (Action, backspace)

-- TODO: Add the menu buttons and rethink what other content is needed in the
--      .glade for this GUI. Oskar Mendel 2018-02-27
data HDocsGUI = HDocsGUI {
    hdocsWnd :: Window,
    hdocsEditor :: TextView
}

loadHDocsGlade :: FilePath -> IO HDocsGUI
loadHDocsGlade gladePath = do
    builder <- builderNew
    builderAddFromFile builder gladePath

    window <- builderGetObject builder castToWindow "HDocs"
    editor <- builderGetObject builder castToTextView "HDocsEditor"

    return $ HDocsGUI window editor

connectHDocsGUI :: HDocsGUI -> IO (ConnectId Window)
connectHDocsGUI gui = do
    on (hdocsWnd gui) objectDestroy mainQuit

main :: FilePath -> IO ()
main gladePath = do
    initGUI

    gui <- loadHDocsGlade gladePath

    connectHDocsGUI gui

    widgetShowAll (hdocsWnd gui)
    mainGUI