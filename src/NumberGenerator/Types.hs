module NumberGenerator.Types
    ( OutputType (..)
    , Encoding (..)
    , PRNGInput (..)
    ) where

data OutputType = Bytes | Int | UUID
    deriving (Read, Show)

data Encoding = ASCII | UTF8 | Base64 | Binary | Hex
    deriving (Read, Show)

data PRNGInput = PRNGInput
    { size          :: Int
    , maxSize       :: Int
    , minSize       :: Int
    , encoding      :: Encoding
    , outputType    :: OutputType
    } deriving (Show)
