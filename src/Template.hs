{-# LANGUAGE OverloadedStrings #-}

module Template where

import Data.Text
import Data.Aeson
import Control.Monad

data TemplateFormat = TemplateFormat
    {
        title       :: Text,
        creator     :: Text,
        author      :: Text,
        documentHeader :: [Text],
        header      :: Text,
        footer      :: Text,
        topMargin   :: Int,
        rightMargin :: Int,
        bottomMargin:: Int,
        leftMargin  :: Int,
        size        :: Text
    } deriving (Show)

data TemplateSection = TemplateSection
    {
        sectionTitle   :: Text,
        sectionContent :: Text
    } deriving (Show)

data TemplateContent = TemplateContent
    {
        sections    :: [TemplateSection]
    } deriving (Show)

data TemplateJSON = TemplateJSON
    {
        format  :: TemplateFormat,
        content :: TemplateContent
    } deriving (Show)

instance FromJSON TemplateFormat where
    parseJSON (Object v) =
        TemplateFormat <$> v .: "title"
                       <*> v .: "creator"
                       <*> v .: "author"
                       <*> v .: "doc-header"
                       <*> v .: "header"
                       <*> v .: "footer"
                       <*> v .: "top-margin"
                       <*> v .: "right-margin"
                       <*> v .: "bottom-margin"
                       <*> v .: "left-margin"
                       <*> v .: "size"
    parseJSON _ = mzero

instance FromJSON TemplateSection where
    parseJSON (Object v) =
        TemplateSection <$> v .: "title"
                        <*> v .: "content"
    parseJSON _ = mzero

instance FromJSON TemplateContent where
    parseJSON (Object v) = TemplateContent <$> v .: "sections"
    parseJSON _ = mzero

instance FromJSON TemplateJSON where
    parseJSON (Object v) =
        TemplateJSON <$> v .: "Template-Format"
                     <*> v .: "Template-Content"
    parseJSON _ = mzero

type Template = Text

wrapTemplateVar :: String -> String
wrapTemplateVar x = "${" ++ x ++ "}"

template :: String -> Template
template x = pack x

templateSet :: Template -> String -> String -> Template
templateSet xs var val = replace packedvar packedval xs
    where
        packedvar = pack $ wrapTemplateVar var
        packedval = pack val