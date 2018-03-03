module Tags (getTagTable) where

import Graphics.UI.Gtk.Multiline.TextTagTable
import Graphics.UI.Gtk.Multiline.TextTag
import System.Glib.Attributes
import Data.Text
import Graphics.Rendering.Pango.Enums

getTagTable :: TextTagTableClass self => self -> IO ()
getTagTable self = do
    boldTag <- createBoldTag
    italicTag <- createItalicTag
    headerTag <- createHeaderTag

    textTagTableAdd self boldTag
    textTagTableAdd self italicTag
    textTagTableAdd self headerTag

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