module TextTags where

import Data.Text
import Graphics.UI.Gtk -- .Multiline.TextTag
import Graphics.Rendering.Pango.Enums


-- TODO: Is this a proper way to store the tags or should I rethink this?
--      Oskar Mendel 2018-03-02
data Tags = Tags
    {
        boldTag     :: TextTag,
        italicTag   :: TextTag,
        headerTag   :: TextTag
    }

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