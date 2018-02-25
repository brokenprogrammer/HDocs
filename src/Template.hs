module Template where

import Data.Text

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

-- Template: "Massa text skriven av ${name} med mening att ${purpose}."
-- TemplateSet "name" "Oskar Mendel"
-- TemplateSet "purpose" "Förenkla skolan.."