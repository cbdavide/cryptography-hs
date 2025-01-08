module RandomNumberGenerator.CLI
(
  parseNumberGeneratorInput
) where

import Options.Applicative

import RandomNumberGenerator.Types (RNGInput(..) , Encoding(..), OutputType (ByteString))

parseNumberGeneratorInput :: Parser RNGInput
parseNumberGeneratorInput = RNGInput
    <$> option auto
        (  long "number-of-bytes"
        <> short 'n'
        <> value 32
        <> help "Number of bytes to generate. Applies for the ByteString output type. (Default: 32)"
        )
    <*> option auto
        (  long "encoding"
        <> short 'e'
        <> value Hex
        <> help "Encoding of the output data. Applies for the ByteString output (Default: Hex)"
        )
    <*> option auto
        (  long "output-type"
        <> short 't'
        <> value ByteString
        <> help "Output type (Default: ByteString)"
        )
    <*> option auto
        (  long "max-number"
        <> value (-1)
        <> help "Interval max. Applies fro Number output type."
        )
    <*> option auto
        (  long "min-number"
        <> value 0
        <> help "Interval min. Applies fro Number output type."
        )
