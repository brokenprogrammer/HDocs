module Tags (getTagTable) where

import Graphics.UI.Gtk.Multiline.TextTagTable
import Graphics.UI.Gtk.Multiline.TextTag
import System.Glib.Attributes
import Data.Text
import Graphics.Rendering.Pango.Enums

getTagTable :: IO TextTagTable
getTagTable = do
    tagTable <- textTagTableNew
    boldTag <- createBoldTag
    italicTag <- createItalicTag
    headerTag <- createHeaderTag
    
    textTagTableAdd tagTable boldTag
    textTagTableAdd tagTable italicTag
    textTagTableAdd tagTable headerTag

    return tagTable

createBoldTag :: IO TextTag
createBoldTag = do
    t <- textTagNew (Just $ pack "BoldTag")
    set t [
            textTagWeightSet := True,
            textTagWeight := fromEnum WeightBold
          ]
    return t

createItalicTag :: IO TextTag
createItalicTag = do
    t <- textTagNew (Just $ pack "ItalicTag")
    set t [
            textTagStyle := StyleItalic
          ]
    return t

createHeaderTag :: IO TextTag
createHeaderTag = do
    t <- textTagNew (Just $ pack "HeaderTag")
    set t [
            textTagSizeSet := True,
            textTagSize := 24
          ]
    return t