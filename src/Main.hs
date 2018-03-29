module Main where

import qualified SplashGUI
import qualified HDocsGUI
import qualified File
import Data.Aeson

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

toDataPath :: String -> String
toDataPath x = "./data/" ++ (filter (/=' ') x) ++ ".json"

main :: IO ()
main = do
    result <- SplashGUI.main "./data/Splash.glade"
    jsonDate <- decode <$> File.readFile (toDataPath (thrd result))
    case jsonDate of
        (Just jsonDate) -> HDocsGUI.main "./data/HDocs.glade" jsonDate
        Nothing         -> print "Nothing"

    -- TODO: This is here for debugging purposes. Oskar Mendel 2018-03-29
    -- jsonDate <- decode <$> File.readFile "./data/ProjectPlan.json"
    -- case jsonDate of
    --     (Just jsonDate) -> HDocsGUI.main "./data/HDocs.glade" jsonDate
    --     Nothing         -> print "Nothing"