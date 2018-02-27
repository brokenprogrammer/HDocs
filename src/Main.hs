module Main where

import qualified SplashGUI
import qualified HDocsGUI

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
    result <- SplashGUI.main "./data/Splash.glade" 


-- TODO: Just nu returnerar SplashGUI ett SplashGUIState som en IO ref
    -- Jag måste ta ut bool från den och kolla om nytt proj ska skapas samt
    -- ta ut dess proj data och verifiera så det är valid..  HUR ?
    -- Oskar Mendel 2018-02-26

    case (fsrt result) of 
        --True -> putStrLn ((scnd result) ++ " " ++ (thrd result))
        True -> HDocsGUI.main "./data/HDocs.glade"
        False -> putStrLn "NO NEW"

    -- case result of 
    --     True -> putStrLn "New Project under way.."
    --     False -> putStrLn "Time to close.."
    -- initGUI

    -- -- Load glade file
    -- builder <- builderNew
    -- builderAddFromFile builder "./data/Splash.glade"

    -- -- Get a handle to some of the widgets of the glade file.
    -- window <- builderGetObject builder castToWindow "SplashWnd"
    -- btnNew <- builderGetObject builder castToButton "SplashNew"
    -- btnEdt <- builderGetObject builder castToButton "SplashEdit"
    -- btnQut <- builderGetObject builder castToButton "SplashQuit"

    -- -- set window [ windowTitle := "HDocs",
    -- --              windowResizable := False,
    -- --              windowDefaultWidth := 800,
    -- --              windowDefaultHeight := 600 ]

    -- -- Do something with the widgets to test..
    -- on btnNew buttonActivated (putStrLn "NEW ITEM.")
    -- on btnEdt buttonActivated test
    -- on btnQut buttonActivated mainQuit
    -- -- When the window is destroeyd quit the application.
    -- on window objectDestroy mainQuit

    -- widgetShowAll window
    -- mainGUI