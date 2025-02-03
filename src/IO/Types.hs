module IO.Types
(
  Input(..)
, Output(..)
) where

data Input = Stdin | InputFile FilePath
    deriving (Read, Show)

data Output = Stdout | OutputFile FilePath
    deriving (Read, Show)
