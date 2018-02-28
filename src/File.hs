module File where

import qualified Data.Text.IO as T (readFile, writeFile)
import qualified Data.Text    as T
import Control.Monad

createFile :: FilePath -> IO ()
createFile name = T.writeFile name $ T.pack ""

saveFile :: FilePath -> T.Text -> IO ()
saveFile path content = T.writeFile path content

readFile :: FilePath -> IO T.Text
readFile path = do
    fileContent <- T.readFile path
    return $ fileContent
