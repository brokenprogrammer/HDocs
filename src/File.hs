module File where

import qualified Data.ByteString.Lazy as B
import Control.Monad

createFile :: FilePath -> IO ()
createFile name = B.writeFile name $ B.empty

saveFile :: FilePath -> B.ByteString -> IO ()
saveFile path content = B.writeFile path content

readFile :: FilePath -> IO B.ByteString
readFile path = do
    fileContent <- B.readFile path
    return $ fileContent
