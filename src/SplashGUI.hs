{-# OPTIONS_GHC -fno-warn-unused-do-bind#-}

module SplashGUI where

import Graphics.UI.Gtk hiding (Action, backspace)
import Data.Text as Text
import Data.IORef
import Control.Monad

data SplashGUI = SplashGUI {
    splashWnd    :: Window,
    splashNew    :: Button,
    splashEdt    :: Button,
    splashSet    :: Button,
    splashQut    :: Button
}

data NewDialogGUI = NewDialogGUI {
    dialogWnd       :: Dialog,
    dialogNme       :: Entry,
    dialogTemplate  :: ComboBox,
    dialogOk        :: Button,
    dialogCnl       :: Button
}

data NewProjectData = NewProjectData {
    projectName     :: String,
    projectTemplate :: Maybe Text
}

data SplashGUIState = SplashGUIState {
    shouldCreateProject :: Bool,
    projectData         :: Maybe NewProjectData
}

loadSplashGlade :: FilePath -> IO SplashGUI
loadSplashGlade gladePath = do
    builder <- builderNew
    builderAddFromFile builder gladePath

    -- Load Splash Window
    window <- builderGetObject builder castToWindow "SplashWnd"

    [btnNew, btnEdt, btnSet, btnQut] <-
        mapM (builderGetObject builder castToButton)
        ["SplashNew", "SplashEdit", "SplashSettings", "SplashQuit"]

    return $ SplashGUI window btnNew btnEdt btnSet btnQut

connectSplashGUI :: SplashGUI -> (IORef SplashGUIState) -> IO (ConnectId Button)
connectSplashGUI gui state = do
    on (splashWnd gui) objectDestroy mainQuit

    on (splashNew gui) buttonActivated (splashNewAction state)
    on (splashEdt gui) buttonActivated (putStrLn "Edit")
    on (splashSet gui) buttonActivated (putStrLn "Settings")
    on (splashQut gui) buttonActivated mainQuit

loadNewDialogGlade :: FilePath -> IO NewDialogGUI
loadNewDialogGlade gladePath = do
    builder <- builderNew
    builderAddFromFile builder gladePath

    -- TODO: Fix warning. Oskar Mendel 2018-02-26
    -- Warning: Gtk-Message: GtkDialog mapped without a transient parent.
        -- This is discouraged.
    dialog <- builderGetObject builder castToDialog "NewDialog"

    entr <- builderGetObject builder castToEntry "NewDialogName"
    combo <- builderGetObject builder castToComboBox "NewDialogTemplate"

    -- Initialize buttons to Stock buttons using the ResponseIds provided by gtk2hs.
    btnOk <- dialogAddButton dialog stockOk ResponseOk
    btnCncl <- dialogAddButton dialog stockCancel ResponseCancel

    return $ NewDialogGUI dialog entr combo btnOk btnCncl

connectDialogGUI :: NewDialogGUI -> IO Int
connectDialogGUI dialog = do

    -- TODO: Add this to data file or other structure. Oskar Mendel 2018-02-26
    -- Has to be initializes through haskell.
    -- Source: https://stackoverflow.com/questions/11990554/combobox-in-gtk2hs-glade
    comboBoxSetModelText (dialogTemplate dialog)
    comboBoxAppendText (dialogTemplate dialog) $ pack "Project Plan"
    comboBoxAppendText (dialogTemplate dialog) $ pack "Test Plan"

splashNewAction :: (IORef SplashGUIState) -> IO ()
splashNewAction state = do
    dialog <- loadNewDialogGlade "./data/NewDialog.glade"

    -- TODO: Oskar Mendel 2018-02-26
    -- If you want to block waiting for a dialog to return 
    -- before returning control flow to your code, you can call dialogRun.
    connectDialogGUI dialog

    result <- dialogRun (dialogWnd dialog)
    name <- (entryGetText (dialogNme dialog))
    template <- (comboBoxGetActiveText (dialogTemplate dialog))

    putStrLn name

    case result of 
        ResponseOk          -> writeIORef state (SplashGUIState True (Just $ NewProjectData name template))
        ResponseCancel      -> writeIORef state (SplashGUIState False Nothing)
        _                   -> writeIORef state (SplashGUIState False Nothing)

    widgetDestroy (dialogWnd dialog)
    mainQuit

-- TODO: This function is error prone and shouldn't be used in evalState.
--  Remove and integrate within evalState. Oskar Mendel 2018-02-27
fromMaybe :: Maybe a -> a
fromMaybe Nothing = error ""
fromMaybe (Just x) = x

-- TODO: Account for when invalid Name or TemplateName is specified.
-- TODO: Maybe remove data types such as SplashGUIState & NewProjectData
--      and use type instead.
--  Oskar Mendel 2018-02-27
evalState :: SplashGUIState -> (Bool, String, String)
evalState x = (shouldCreate , projName, templateName)
        where 
            shouldCreate = (shouldCreateProject x)
            projData = fromMaybe (projectData x)
            projName = (projectName projData)
            templateName = unpack (fromMaybe (projectTemplate projData))


main :: FilePath -> IO (Bool, String, String)
main gladePath = do
    initGUI

    -- State ( Mutable references )
    state <- newIORef (SplashGUIState False Nothing)

    -- Initialize Splash screen GUI from the glade path.
    gui <- loadSplashGlade gladePath

    -- Connect the GUI to actionlisteners
    connectSplashGUI gui state

    widgetShowAll (splashWnd gui)
    mainGUI

    evaluatedState <- liftM evalState $ (readIORef state)
    widgetDestroy (splashWnd gui)
    return evaluatedState