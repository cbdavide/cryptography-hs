module Hash.CLI
(
  parseHashInput
) where

import IO.Types (Input(..), Output(..))
import IO.Options
import Options.Applicative

import qualified Hash.Types as Types
import Data.List (intercalate)

parseHashInput :: Parser Types.HashInput
parseHashInput = Types.HashInput
    <$> option auto
        (  short 'a'
        <> long "algorithm"
        <> value Types.SHA3
        <> help "Hash algorithm"
        <> showDefault
        )
    <*> option outputSizeReader
        (  short 's'
        <> long "digest-size"
        <> value Types.Size256
        <> help ("Size of the digest: " ++ validOuptputSizes)
        <> showDefaultWith (const "256")
        )
    <*> withStdinInputOption inputFileOption
    <*> withStdoutOutputOption outputFileOption
    <*> option auto
        (  short 'e'
        <> long "encoding"
        <> value Types.Hex
        <> help "Encoding of the digest: Hex, Base64"
        <> showDefault
        )


inputFileOption :: Parser Input
inputFileOption = InputFile <$>
    strOption
        (  short 'i'
        <> long "input"
        <> metavar "FILE"
        <> help "File where the input is located (default: stdin)"
        )

outputFileOption :: Parser Output
outputFileOption = OutputFile <$>
    strOption
        (  short 'o'
        <> long "output"
        <> metavar "FILE"
        <> help "File where the output will be stored (default: stdout)"
        )

outputSizeReader :: ReadM Types.OutputSize
outputSizeReader = eitherReader $ \arg -> case arg of
    "224" -> return Types.Size224
    "256" -> return Types.Size256
    "384" -> return Types.Size384
    "512" -> return Types.Size512
    _     -> Left $ "Invalid output size '" ++ arg ++ "'. The valid values are: " ++ validOuptputSizes

validOuptputSizes :: String
validOuptputSizes = intercalate ", " (fmap show validSizes)
    where validSizes = enumFrom minBound :: [Types.OutputSize]
