module Main where

import qualified SplashGUI
import qualified HDocsGUI
import qualified File
import Data.Aeson
import Template

-- TOOD: 1. New knapp
    -- Steps: 
        -- 1. Användare klickar på New
        -- 2. Message dialog, frågar efter Namn och Template (Template från dropdown)
        -- 3. Användare väljer och klickar okej
        -- 4. Visa main fönstret med laddad tempalte.

-- TODO: If these functions should still be used this is not the right module
--  for them. Oskar Mendel 2018-02-27
fsrt :: (a, b, c) -> a
fsrt (x, _, _) = x

scnd :: (a, b, c) -> b
scnd (_, x, _) = x

thrd :: (a, b, c) -> c
thrd (_, _, x) = x

main :: IO ()
main = do
    jsonDate <- decode <$> File.readFile "./data/TestTemplateSimple.json"

    print (jsonDate :: Maybe TemplateJSON)

    -- result <- SplashGUI.main "./data/Splash.glade" 

    -- case (fsrt result) of 
    --     --True -> putStrLn ((scnd result) ++ " " ++ (thrd result))
    --     True -> HDocsGUI.main "./data/HDocs.glade"
    --     False -> putStrLn "NO NEW"