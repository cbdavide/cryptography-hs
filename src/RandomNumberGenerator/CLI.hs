module RandomNumberGenerator.CLI
(
  parseNumberGeneratorInput
) where

import Options.Applicative

import RandomNumberGenerator.Types (RNGInput(..) , Encoding(..))

parseNumberGeneratorInput :: Parser RNGInput
parseNumberGeneratorInput = RNGInput
    <$> option auto
        (  long "size"
        <> short 's'
        <> help "Output size"
        )
    <*> option auto
        (  long "encoding"
        <> short 's'
        <> value Hex
        <> help "Encoding of the output data (Default: Hex)"
        )
