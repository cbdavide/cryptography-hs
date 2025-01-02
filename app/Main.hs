module Main (main) where

import NumberGenerator.CLI
import NumberGenerator.Types (PRNGInput(..))

import Options.Applicative

data InputData = Number PRNGInput | Undefined
    deriving (Show)


parseInputData :: ParserInfo InputData
parseInputData = info (commands  <**> helper)
        (  fullDesc
        <> progDesc "Cryptography utils")
    where commands = subparser
            (command "prng"
                (info ((Number <$> parseNumberGeneratorInput) <**> helper)
                (progDesc "Generate a pseudo-random number"))
            )

main :: IO ()
main = process =<< customExecParser (prefs showHelpOnEmpty) parseInputData

process :: InputData -> IO ()
process (Number d) = print d
process _ = undefined
