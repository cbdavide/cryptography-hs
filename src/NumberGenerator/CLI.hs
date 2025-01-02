module NumberGenerator.CLI
    ( parseNumberGeneratorInput
    ) where

import Options.Applicative

import NumberGenerator.Types
    ( PRNGInput (..)
    , Encoding (..)
    , OutputType (..)
    )

parseNumberGeneratorInput :: Parser PRNGInput
parseNumberGeneratorInput = PRNGInput
    <$> option auto
        (  long "size"
        <> short 's'
        <> help "Output size"
        )
    <*> option auto
        (  long "max-size"
        )
    <*> option auto
        (  long "min-size"
        )
    <*> option auto
        (  long "encoding"
        <> short 's'
        <> value Hex
        <> help "Encoding of the output data (Default: Hex)"
        )
    <*> option auto
        (  long "type"
        <> short 't'
        <> help "Output data type (Default: Bytes)"
        <> value Bytes
        )
