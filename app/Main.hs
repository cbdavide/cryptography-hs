module Main (main) where

import AESCipher.Types (AESCipherInput)
import AESCipher.CLI (parseAESCipherInput)
import RandomNumberGenerator.CLI
import RandomNumberGenerator.Types (RNGInput(..))
import RandomNumberGenerator.Handler (processRNGCommand)

import Options.Applicative

data InputData = RandomNumber RNGInput | AESCipher AESCipherInput
    deriving (Show)


parseInputData :: ParserInfo InputData
parseInputData = info (commands  <**> helper)
        (  fullDesc
        <> progDesc "Cryptography utils")
    where commands = subparser
            (command "rng"
                (info ((RandomNumber <$> parseNumberGeneratorInput) <**> helper)
                (progDesc "Generate a random number"))
            <> command "aes-cipher"
                (info ((AESCipher <$> parseAESCipherInput) <**> helper)
                (progDesc "Encrypt or decrypt using AES block cipher"))
            )

main :: IO ()
main = process =<< customExecParser (prefs showHelpOnEmpty) parseInputData

process :: InputData -> IO ()
process (RandomNumber input) = processRNGCommand input
process (AESCipher _) = putStrLn "Hey!"
