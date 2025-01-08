module RandomNumberGenerator.Types
(
  Encoding (..)
, OutputType (..)
, RNGInput (..)
) where

data OutputType = Number | ByteString
    deriving (Read, Show)

data Encoding = UTF8 | Base64 | Binary | Hex
    deriving (Read, Show)

data RNGInput = RNGInput
    { numberOfBytes :: Int
    , encoding      :: Encoding
    , outputType    :: OutputType
    , maxNumber     :: Int
    , minNumber      :: Int
    } deriving (Read, Show)
