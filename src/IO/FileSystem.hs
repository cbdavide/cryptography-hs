module IO.FileSystem
(
  readInput
, writeOutput
) where

import IO.Types
import System.IO (Handle, hClose, IOMode (..), stdin, stdout, openFile)

import qualified Data.ByteString as BS

readInput :: Input -> IO BS.ByteString
readInput input' = do
    handle <- getInputHandle input'
    inputData <- BS.hGetContents handle
    hClose handle
    return inputData

writeOutput :: Output -> BS.ByteString -> IO ()
writeOutput output' data' = do
    handle <- getOutputHandle output'
    BS.hPut handle data'
    hClose handle

getInputHandle :: Input -> IO Handle
getInputHandle Stdin = return stdin
getInputHandle (InputFile filePath) = openFile filePath ReadMode

getOutputHandle :: Output -> IO Handle
getOutputHandle Stdout = return stdout
getOutputHandle (OutputFile filePath) = openFile filePath WriteMode
