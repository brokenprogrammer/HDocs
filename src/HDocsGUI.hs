{-# OPTIONS_GHC -fno-warn-unused-do-bind#-}

module HDocsGUI where

import Graphics.UI.Gtk hiding (Action, backspace)
import Data.Text
import Template

-- TODO: Add the menu buttons and rethink what other content is needed in the
--      .glade for this GUI. Oskar Mendel 2018-02-27
data HDocsGUI = HDocsGUI {
    hdocsWnd :: Window,
    hdocsEditor :: TextView,
    hdocsEditorBuffer :: TextBuffer,
    hdocsEditorBufferItr :: TextIter
}

loadHDocsGlade :: FilePath -> IO HDocsGUI
loadHDocsGlade gladePath = do
    builder <- builderNew
    builderAddFromFile builder gladePath

    window <- builderGetObject builder castToWindow "HDocs"
    editor <- builderGetObject builder castToTextView "HDocsEditor"
    buffer <- textViewGetBuffer editor
    buffitr <- textBufferGetStartIter buffer

    return $ HDocsGUI window editor buffer buffitr

connectHDocsGUI :: HDocsGUI -> IO (ConnectId Window)
connectHDocsGUI gui = do
    on (hdocsWnd gui) objectDestroy mainQuit

addToBuffer :: HDocsGUI -> Text -> IO ()
addToBuffer gui target = do
    textBufferInsert (hdocsEditorBuffer gui) (hdocsEditorBufferItr gui) (append target $ pack "\n")

populateHDocsGUI :: HDocsGUI -> TemplateJSON -> IO [()]
populateHDocsGUI gui jsonTemplate = do
    -- TODO: This can certanly be implemented in a better way.. 
    --  I need to figure something out. Oskar Mendel 2018-03-01
    mapM (\x -> addToBuffer gui (append (append (sectionTitle x) $ pack "\n") (sectionContent x)))
        ((sections (content jsonTemplate)))

main :: FilePath -> TemplateJSON -> IO ()
main gladePath jsonTemplate = do
    initGUI

    gui <- loadHDocsGlade gladePath

    connectHDocsGUI gui
    populateHDocsGUI gui jsonTemplate

    widgetShowAll (hdocsWnd gui)
    mainGUI