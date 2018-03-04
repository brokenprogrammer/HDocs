{-# OPTIONS_GHC -fno-warn-unused-do-bind#-}

module HDocsGUI where

import Graphics.UI.Gtk hiding (Action, backspace)
import Data.Text (Text, pack, unpack, append)
import Template
import Tags

-- TODO: Add the menu buttons and rethink what other content is needed in the
--      .glade for this GUI. Oskar Mendel 2018-02-27
data HDocsGUI = HDocsGUI {
    hdocsWnd                :: Window,
    hdocsLinks              :: TreeView,
    hdocsLinksStore         :: ListStore String,
    hdocsVars               :: TreeView,
    hdocsVarsStore          :: ListStore String,
    hdocsEditor             :: TextView,
    hdocsEditorBuffer       :: TextBuffer,
    hdocsEditorBufferItr    :: TextIter
}

loadHDocsGlade :: FilePath -> IO HDocsGUI
loadHDocsGlade gladePath = do
    builder <- builderNew
    builderAddFromFile builder gladePath

    -- Initialize objects from glade
    window <- builderGetObject builder castToWindow "HDocs"
    links <- builderGetObject builder castToTreeView "HDocsLinks"
    linksColumn <- builderGetObject builder castToTreeViewColumn "LinkName"
    vars <- builderGetObject builder castToTreeView "HDocsVars"
    varsColumn <- builderGetObject builder castToTreeViewColumn "VarName"
    editor <- builderGetObject builder castToTextView "HDocsEditor"
    buffer <- textViewGetBuffer editor
    buffitr <- textBufferGetStartIter buffer

    -- Initialize Text Tags
    table <- textBufferGetTagTable buffer
    getTagTable table

    -- Initialize Link model.
    linksStore <- listStoreNew []
    treeViewSetModel links linksStore

    -- Initialize Vars model.
    varsStore <- listStoreNew []
    treeViewSetModel vars varsStore

    -- Initialize renderers for the cells in the two models.
    linksRenderer <- cellRendererTextNew
    varsRenderer <- cellRendererTextNew

    -- Packs cell renderers into the cell layout.
    cellLayoutPackStart linksColumn linksRenderer True
    cellLayoutPackStart varsColumn varsRenderer True
    
    -- Helper function that sets the cellText attribute of a cell.
    let setCellText = \ind -> [cellText := ind]

    -- Specifying how a row in the model defines its attributes on a cell.
    cellLayoutSetAttributes linksColumn linksRenderer linksStore setCellText
    cellLayoutSetAttributes varsColumn varsRenderer varsStore setCellText


    return $ HDocsGUI window links linksStore vars varsStore editor buffer buffitr

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

populateHDocsGUI :: HDocsGUI -> TemplateJSON -> IO Int
populateHDocsGUI gui jsonTemplate = do
    table <- (textBufferGetTagTable (hdocsEditorBuffer gui))
    tag <- textTagTableLookup table "BoldTag"

    mapM_ (\x -> do
        addToBuffer gui (sectionTitle x) tag
        addToBuffer gui (sectionContent x) Nothing
        listStoreAppend (hdocsLinksStore gui) (unpack $ sectionTitle x))
        ((sections (content jsonTemplate)))

    -- TODO: Append the actual variables by looping through the entire JSON
    --  contents.. Oskar Mendel 2018-03-04
    listStoreAppend (hdocsVarsStore gui) ("Some Var") 

main :: FilePath -> TemplateJSON -> IO ()
main gladePath jsonTemplate = do
    initGUI

    gui <- loadHDocsGlade gladePath

    connectHDocsGUI gui
    populateHDocsGUI gui jsonTemplate

    widgetShowAll (hdocsWnd gui)
    mainGUI