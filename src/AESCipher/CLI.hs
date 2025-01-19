module AESCipher.CLI
(
  parseAESCipherInput
) where

import Data.Maybe (fromMaybe)

import Options.Applicative
import qualified AESCipher.Types as Types

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
    <*> strOption
        (  short 'k'
        <> long "key"
        <> help "Key to encrypt or decrypt the input data"
        )
    <*> inputOption
    <*> outputOption

inputOption :: Parser Types.Input
inputOption = fromMaybe Types.Stdin <$> optional inputFileOption

inputFileOption :: Parser Types.Input
inputFileOption = Types.InputFile <$>
    strOption
        (  short 'i'
        <> long "input"
        <> metavar "FILE"
        <> help "File where the plain/cipher text is located (default: stdin)"
        )

outputOption :: Parser Types.Output
outputOption = fromMaybe Types.Stdout <$> optional outputFileOption

outputFileOption :: Parser Types.Output
outputFileOption = Types.OutputFile <$>
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
