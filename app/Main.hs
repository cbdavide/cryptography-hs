module Main (main) where

import RandomNumberGenerator.CLI
import RandomNumberGenerator.Types (RNGInput(..))

import Options.Applicative

data InputData = Number RNGInput | Undefined
    deriving (Show)


parseInputData :: ParserInfo InputData
parseInputData = info (commands  <**> helper)
        (  fullDesc
        <> progDesc "Cryptography utils")
    where commands = subparser
            (command "rng"
                (info ((Number <$> parseNumberGeneratorInput) <**> helper)
                (progDesc "Generate a random number"))
            )

main :: IO ()
main = process =<< customExecParser (prefs showHelpOnEmpty) parseInputData

process :: InputData -> IO ()
process (Number d) = print d
process _ = undefined
