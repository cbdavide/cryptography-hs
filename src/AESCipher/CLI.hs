module AESCipher.CLI
(
  parseAESCipherInput
) where

import Data.Char (isHexDigit)
import Options.Applicative
import IO.Options ( withStdinInputOption, withStdoutOutputOption )
import IO.Types (Input(..), Output(..))
import qualified AESCipher.Types as Types
import qualified Data.Text as T

parseAESCipherInput :: Parser Types.AESCipherInput
parseAESCipherInput = Types.AESCipherInput
    <$> operationOption
    <*> option keySizeReader
        (  short 's'
        <> long "key-size"
        <> value Types.KeySize128
        <> help "Size of the key: 128 | 192 | 256"
        <> showDefaultWith (const "128")
        )
    <*> option keyReader
        (  short 'k'
        <> long "key"
        <> help "Key to encrypt or decrypt the input data"
        )
    <*> option nonceReader
        (  short 'n'
        <> long "nonce"
        <> help "Nonce used to encrypt or decrypt the input data"
        )
    <*> withStdinInputOption inputFileOption
    <*> withStdoutOutputOption outputFileOption


inputFileOption :: Parser Input
inputFileOption = InputFile <$>
    strOption
        (  short 'i'
        <> long "input"
        <> metavar "FILE"
        <> help "File where the plain/cipher text is located (default: stdin)"
        )

outputFileOption :: Parser Output
outputFileOption = OutputFile <$>
    strOption
        (  short 'o'
        <> long "output"
        <> metavar "FILE"
        <> help "File where the plain/cipher text will be stored (default: stdout)"
        )

operationOption :: Parser Types.CipherOperation
operationOption = encryptOperationFlag <|> decryptOperationFlag

encryptOperationFlag :: Parser Types.CipherOperation
encryptOperationFlag = flag' Types.Encrypt
    (  short 'e'
    <> long "encrypt"
    <> help "Encrypt the input data"
    )

decryptOperationFlag :: Parser Types.CipherOperation
decryptOperationFlag = flag' Types.Decrypt
    (  short 'd'
    <> long "decrypt"
    <> help "Decrypt the input data"
    )

keySizeReader :: ReadM Types.KeySize
keySizeReader = eitherReader $ \arg -> case arg of
    "128" -> return Types.KeySize128
    "192" -> return Types.KeySize192
    "256" -> return Types.KeySize256
    _     -> Left $ "invalid key size '" ++ arg ++ "'. The valid values are: 128, 192, 256"

keyReader :: ReadM T.Text
keyReader = eitherReader $ \arg -> validate arg
    where validate input
            | all isHexDigit input = return $ T.pack input
            | otherwise = Left "Invalid input, the key must be a valid hexadecimal string"

nonceReader :: ReadM T.Text
nonceReader = eitherReader $ \arg -> validate arg
    where validate input
            | length input /= 32 = Left $ "Invalid nonce length: expected 32 hex characters, but got " ++ show (length input)
            | not (all isHexDigit input) = Left "Invalid input, the nonce must be a valid hexadecimal string"
            | otherwise = return $ T.pack input
