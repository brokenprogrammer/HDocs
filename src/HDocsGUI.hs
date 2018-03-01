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
    hdocsEditorBufferItr :: TextIter,
    hdocsTag :: TextTag
}

myTag :: IO TextTag
myTag = do
    someTag <- textTagNew (Just $ pack "MYbold")
    set someTag [
        textTagWeightSet := True,
        textTagWeight := 800
        ]
    return someTag

loadHDocsGlade :: FilePath -> IO HDocsGUI
loadHDocsGlade gladePath = do
    builder <- builderNew
    builderAddFromFile builder gladePath

    window <- builderGetObject builder castToWindow "HDocs"
    editor <- builderGetObject builder castToTextView "HDocsEditor"
    buffer <- textViewGetBuffer editor
    buffitr <- textBufferGetStartIter buffer
    tag <- myTag

    table <- (textBufferGetTagTable buffer)
    textTagTableAdd table tag

    return $ HDocsGUI window editor buffer buffitr tag

connectHDocsGUI :: HDocsGUI -> IO (ConnectId Window)
connectHDocsGUI gui = do
    on (hdocsWnd gui) objectDestroy mainQuit

addToBuffer :: HDocsGUI -> Text -> IO ()
addToBuffer gui target = do
    textBufferInsert (hdocsEditorBuffer gui) (hdocsEditorBufferItr gui) (append target $ pack "\n")

populateHDocsGUI :: HDocsGUI -> TemplateJSON -> IO ()
populateHDocsGUI gui jsonTemplate = do
    -- TODO: This can certanly be implemented in a better way.. 
    --  I need to figure something out. Oskar Mendel 2018-03-01
    mapM (\x -> addToBuffer gui (append (append (sectionTitle x) $ pack "\n") (sectionContent x)))
        ((sections (content jsonTemplate)))

    --TODO: Place this within a data structure.. With haskells lazyness this can be 
    --  easily retrieved instead of using the actual TagTable.. ? Oskar Mendel 2018-03-01
    ttable <- (textBufferGetTagTable (hdocsEditorBuffer gui))
    aTag <- textTagTableLookup ttable "MYbold"
    -- TODO: Get offset at current word and then get iter after the current word then we can
    --  implement the addToBuffer function successfully. Oskar Mendel 2018-03-01
    something <- textBufferGetIterAtOffset (hdocsEditorBuffer gui) 0
    somethingElse <- textBufferGetIterAtOffset (hdocsEditorBuffer gui) 12
    case aTag of
        (Just aTag) -> textBufferApplyTag (hdocsEditorBuffer gui) aTag something somethingElse
        Nothing -> putStrLn "Nothing"

main :: FilePath -> TemplateJSON -> IO ()
main gladePath jsonTemplate = do
    initGUI

    gui <- loadHDocsGlade gladePath

    connectHDocsGUI gui
    populateHDocsGUI gui jsonTemplate

    widgetShowAll (hdocsWnd gui)
    mainGUI