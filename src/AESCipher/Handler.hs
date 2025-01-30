module AESCipher.Handler
( processAESCipherHandler
) where

import AESCipher.Types (AESCipherInput (..), KeySize(..), CipherOperation(..), Input (Stdin, InputFile), Output (Stdout, OutputFile))
import Botan.BlockCipher (aes128, aes192, aes256)
import Botan.Cipher (cbc, Cipher, cipherEncrypt, cipherDecrypt)
import Botan.Utility (hexDecode)
import Control.Monad (unless)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe (isJust, fromJust)
import System.IO (stdin, stdout, Handle, openFile, IOMode (ReadMode, WriteMode), hClose)

import qualified Data.Text as T
import qualified Data.ByteString as BS

type PlainText = BS.ByteString
type CipherKey = BS.ByteString
type CipherNonce = BS.ByteString
type CipherText = BS.ByteString

processAESCipherHandler :: AESCipherInput -> ExceptT String IO ()
processAESCipherHandler x = do
    _ <- validateKey x

    text <- liftIO $ readInput (input x)

    let encodedKey = hexDecode $ key x
        encodedNonce = hexDecode $ nonce x

    let result = case operation x of
            Encrypt -> encrypt text encodedKey encodedNonce (keySize x)
            Decrypt -> decrypt text encodedKey encodedNonce (keySize x)

    unless (isJust result) $ do
        throwError $ "Failed to " ++ show (operation x) ++ " the input data."

    liftIO $ writeOutput (output x) (fromJust result)


readInput :: Input -> IO BS.ByteString
readInput input' = do
    handle <- getInputHandle input'
    inputData <- BS.hGetContents handle
    hClose handle
    return inputData

getInputHandle :: Input -> IO Handle
getInputHandle Stdin = return stdin
getInputHandle (InputFile filePath) = openFile filePath ReadMode

writeOutput :: Output -> BS.ByteString -> IO ()
writeOutput output' data' = do
    handle <- getOutputHandle output'
    BS.hPut handle data'
    hClose handle

getOutputHandle :: Output -> IO Handle
getOutputHandle Stdout = return stdout
getOutputHandle (OutputFile filePath) = openFile filePath WriteMode

encrypt :: PlainText -> CipherKey -> CipherNonce -> KeySize -> Maybe CipherText
encrypt plaintext k nce ksize = Just $ cipherEncrypt cipher k nce plaintext
    where cipher = getCipherFromKeySize ksize

decrypt :: CipherText -> CipherKey -> CipherNonce -> KeySize -> Maybe PlainText
decrypt ciphertext k nce ksize = cipherDecrypt cipher k nce ciphertext
    where cipher = getCipherFromKeySize ksize

getCipherFromKeySize :: KeySize -> Cipher
getCipherFromKeySize KeySize128 = cbc aes128
getCipherFromKeySize KeySize192 = cbc aes192
getCipherFromKeySize KeySize256 = cbc aes256

validateKey :: AESCipherInput -> ExceptT String IO ()
validateKey x = do
    let klen = T.length $ key x
        expectedKeyLength = case keySize x of
            KeySize128 -> 32
            KeySize192 -> 48
            KeySize256 -> 64

    unless (klen == expectedKeyLength) $ do
        throwError $ "Invalid key length: expected "
                  ++ show expectedKeyLength
                  ++ " hex characters, but got "
                  ++ show klen
