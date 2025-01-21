module Main (main) where

import Control.Monad.Except
import AESCipher.CLI (parseAESCipherInput)
import AESCipher.Types (AESCipherInput)
import AESCipher.Handler (processAESCipherHandler)
import RandomNumberGenerator.CLI
import RandomNumberGenerator.Types (RNGInput(..))
import RandomNumberGenerator.Handler (processRNGCommand)

import Options.Applicative

data CLICommand a = CLICommand
    { name        :: String
    , parser      :: Parser a
    , modifiers   :: InfoMod a
    }

data InputData = RandomNumber RNGInput | AESCipher AESCipherInput
    deriving (Show)

cliCommands :: [CLICommand InputData]
cliCommands =
    [ CLICommand
        { name = "rng"
        , parser = RandomNumber <$> parseNumberGeneratorInput
        , modifiers = progDesc "Generate a random number"
        }
    , CLICommand
        { name = "aes-cipher"
        , parser = AESCipher <$> parseAESCipherInput
        , modifiers = progDesc "Encrypt or decrypt using AES block cipher"
        }
    ]

main :: IO ()
main = process =<< customExecParser (prefs showHelpOnEmpty) parseInputData

process :: InputData -> IO ()
process (RandomNumber input) = processRNGCommand input
process (AESCipher input) = processExceptTHandler (processAESCipherHandler input)

processExceptTHandler :: ExceptT String IO () -> IO ()
processExceptTHandler x = do
    cmp <- runExceptT x

    case cmp of
        Left err -> fail err
        Right _ -> return ()

parseInputData :: ParserInfo InputData
parseInputData = info (commands  <**> helper) (fullDesc <> progDesc "Cryptography utils")
    where commands = subparser (mconcat $ buildCommand <$> cliCommands)

buildCommand :: CLICommand a -> Mod CommandFields a
buildCommand cmd = command (name cmd) (info (parser cmd <**> helper) (modifiers cmd))
