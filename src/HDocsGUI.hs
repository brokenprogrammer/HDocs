{-# OPTIONS_GHC -fno-warn-unused-do-bind#-}

module HDocsGUI where

import Graphics.UI.Gtk hiding (Action, backspace)
import Data.Text (Text, pack, append)
import Template
import Tags

-- TODO: Add the menu buttons and rethink what other content is needed in the
--      .glade for this GUI. Oskar Mendel 2018-02-27
data HDocsGUI = HDocsGUI {
    hdocsWnd                :: Window,
    hdocsEditor             :: TextView,
    hdocsEditorBuffer       :: TextBuffer,
    hdocsEditorBufferItr    :: TextIter
}

loadHDocsGlade :: FilePath -> IO HDocsGUI
loadHDocsGlade gladePath = do
    builder <- builderNew
    builderAddFromFile builder gladePath

    window <- builderGetObject builder castToWindow "HDocs"
    editor <- builderGetObject builder castToTextView "HDocsEditor"
    buffer <- textViewGetBuffer editor
    buffitr <- textBufferGetStartIter buffer

    -- Initialise Text Tags
    table <- textBufferGetTagTable buffer
    getTagTable table

    return $ HDocsGUI window editor buffer buffitr

connectHDocsGUI :: HDocsGUI -> IO (ConnectId Window)
connectHDocsGUI gui = do
    on (hdocsWnd gui) objectDestroy mainQuit

-- TODO: This function should be cleaned up somehow.. Oskar Mendel 2018-03-01
addToBuffer :: HDocsGUI -> Text -> Maybe TextTag -> IO ()
addToBuffer gui target (Just tag) = do
    start <- textIterGetOffset (hdocsEditorBufferItr gui)
    textBufferInsert (hdocsEditorBuffer gui) (hdocsEditorBufferItr gui) (append target $ pack "\n")
    end <- textIterGetOffset (hdocsEditorBufferItr gui)

    startItr <- textBufferGetIterAtOffset (hdocsEditorBuffer gui) start
    endItr <- textBufferGetIterAtOffset (hdocsEditorBuffer gui) end
    textBufferApplyTag (hdocsEditorBuffer gui) tag startItr endItr
addToBuffer gui target Nothing = do
    textBufferInsert (hdocsEditorBuffer gui) (hdocsEditorBufferItr gui) (append target $ pack "\n")

populateHDocsGUI :: HDocsGUI -> TemplateJSON -> IO ()
populateHDocsGUI gui jsonTemplate = do
    table <- (textBufferGetTagTable (hdocsEditorBuffer gui))
    tag <- textTagTableLookup table "BoldTag"

    mapM_ (\x -> do
        addToBuffer gui (sectionTitle x) tag
        addToBuffer gui (sectionContent x) Nothing)
        ((sections (content jsonTemplate)))

main :: FilePath -> TemplateJSON -> IO ()
main gladePath jsonTemplate = do
    initGUI

    gui <- loadHDocsGlade gladePath

    connectHDocsGUI gui
    populateHDocsGUI gui jsonTemplate

    widgetShowAll (hdocsWnd gui)
    mainGUI