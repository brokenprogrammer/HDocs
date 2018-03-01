{-# OPTIONS_GHC -fno-warn-unused-do-bind#-}

module HDocsGUI where

import Graphics.UI.Gtk hiding (Action, backspace)
import Template

-- TODO: Add the menu buttons and rethink what other content is needed in the
--      .glade for this GUI. Oskar Mendel 2018-02-27
data HDocsGUI = HDocsGUI {
    hdocsWnd :: Window,
    hdocsEditor :: TextView,
    hdocsEditorBuffer :: TextBuffer
}

loadHDocsGlade :: FilePath -> IO HDocsGUI
loadHDocsGlade gladePath = do
    builder <- builderNew
    builderAddFromFile builder gladePath

    window <- builderGetObject builder castToWindow "HDocs"
    editor <- builderGetObject builder castToTextView "HDocsEditor"
    buffer <- textViewGetBuffer editor

    return $ HDocsGUI window editor buffer

connectHDocsGUI :: HDocsGUI -> IO (ConnectId Window)
connectHDocsGUI gui = do
    on (hdocsWnd gui) objectDestroy mainQuit

populateHDocsGUI :: HDocsGUI -> TemplateJSON -> IO ()
populateHDocsGUI gui jsonTemplate = do
    print (sections (content jsonTemplate))

main :: FilePath -> TemplateJSON -> IO ()
main gladePath jsonTemplate = do
    initGUI

    gui <- loadHDocsGlade gladePath

    connectHDocsGUI gui
    populateHDocsGUI gui jsonTemplate

    widgetShowAll (hdocsWnd gui)
    mainGUI