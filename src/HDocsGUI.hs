{-# OPTIONS_GHC -fno-warn-unused-do-bind#-}

module HDocsGUI where

import Graphics.UI.Gtk hiding (Action, backspace)
import Data.Text (Text, pack, unpack, append)
import Data.List
import Data.List.Split
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
    hdocsEditorBufferItr    :: TextIter,
    hdocsHelpBar            :: TextView,
    hdocsLinkComments       :: ListStore String, -- ListStores for the comments
    hdocsVarComments        :: ListStore String  -- is there a better way to do this?
}                                                --     Oskar Mendel 2018-03-08

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
    helpbar <- builderGetObject builder castToTextView "HDocsHelpBar"

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

    -- Create listStores that should hold the comments.
    linkComments <- listStoreNew []
    varComments <- listStoreNew []

    return $ 
        HDocsGUI window links linksStore vars varsStore 
        editor buffer buffitr helpbar linkComments varComments

connectHDocsGUI :: HDocsGUI -> IO (ConnectId TreeSelection)
connectHDocsGUI gui = do
    on (hdocsWnd gui) objectDestroy mainQuit

    linkSelectionModel <- treeViewGetSelection (hdocsLinks gui)
    treeSelectionSetMode linkSelectionModel SelectionSingle
    on linkSelectionModel treeSelectionSelectionChanged 
        (onLinkSelection gui (hdocsLinksStore gui) linkSelectionModel)

    varSelectionModel <- treeViewGetSelection (hdocsVars gui)
    treeSelectionSetMode varSelectionModel SelectionSingle
    on varSelectionModel treeSelectionSelectionChanged
        (onVarSelection gui (hdocsVarsStore gui) varSelectionModel)


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

setHelpBarText :: HDocsGUI -> Text -> IO ()
setHelpBarText gui text = do 
    buffer <- textViewGetBuffer (hdocsHelpBar gui)
    textBufferSetText buffer $ unpack text

-- TODO: Actually show the variable comment for the selected value.
--  Oskar Mendel 2018-03-05
onVarSelection :: HDocsGUI -> ListStore String -> TreeSelection -> IO ()
onVarSelection gui list selection = do
    selected <- treeSelectionGetSelectedRows selection
    let s = (head . head) selected
    value <- listStoreGetValue list s
    setHelpBarText gui $ pack value

onLinkSelection :: HDocsGUI -> ListStore String -> TreeSelection -> IO ()
onLinkSelection gui list selection = do
    selected <- treeSelectionGetSelectedRows selection
    let row = (head . head) selected
    comment <- listStoreGetValue (hdocsLinkComments gui) row
    setHelpBarText gui $ pack comment

populateHDocsGUI :: HDocsGUI -> TemplateJSON -> IO ()
populateHDocsGUI gui jsonTemplate = do
    table <- (textBufferGetTagTable (hdocsEditorBuffer gui))
    tag <- textTagTableLookup table "BoldTag"

    mapM_ (\x -> do
        addToBuffer gui (sectionTitle x) tag
        addToBuffer gui (sectionContent x) Nothing
        listStoreAppend (hdocsLinksStore gui) (unpack $ sectionTitle x)
        listStoreAppend (hdocsLinkComments gui) (unpack $ sectionComment x))
        ((sections (content jsonTemplate)))

    -- Stores all the variables read from the JSON into the variables list store.
    mapM_ (\x -> do
        listStoreAppend (hdocsVarsStore gui) (x)) $ 
        filter (not . null) $
        splitVarStrings $
        map (filter (not . (`elem` "\"[]{},."))) $ 
        filter (\x -> isInfixOf "${" x) $ 
        (words . show) jsonTemplate

-- TODO: Should this function be in this module and could it be made more generalized?
--  Oskar Mendel 2018-03-05
splitVarStrings :: [String] -> [String]
splitVarStrings (x:xs) = if (length $ splitOn "$" x) > 1
    then splitOn "$" x ++ splitVarStrings xs
    else x : splitVarStrings xs
splitVarStrings [] = []

main :: FilePath -> TemplateJSON -> IO ()
main gladePath jsonTemplate = do
    initGUI

    gui <- loadHDocsGlade gladePath

    connectHDocsGUI gui
    populateHDocsGUI gui jsonTemplate

    widgetShowAll (hdocsWnd gui)
    mainGUI