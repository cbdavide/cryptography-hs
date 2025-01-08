module Main (main) where

import RandomNumberGenerator.CLI
import RandomNumberGenerator.Types (RNGInput(..))
import RandomNumberGenerator.Handler (processRNGCommand)
import Options.Applicative

data InputData = RandomNumber RNGInput | Undefined
    deriving (Show)


parseInputData :: ParserInfo InputData
parseInputData = info (commands  <**> helper)
        (  fullDesc
        <> progDesc "Cryptography utils")
    where commands = subparser
            (command "rng"
                (info ((RandomNumber <$> parseNumberGeneratorInput) <**> helper)
                (progDesc "Generate a random number"))
            )

main :: IO ()
main = process =<< customExecParser (prefs showHelpOnEmpty) parseInputData

process :: InputData -> IO ()
process (RandomNumber input) = processRNGCommand input
process _ = undefined
