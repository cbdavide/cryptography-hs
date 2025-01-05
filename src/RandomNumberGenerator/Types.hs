module RandomNumberGenerator.Types
(
  Encoding (..)
, RNGInput (..)
) where

data Encoding = ASCII | UTF8 | Base64 | Binary | Hex
    deriving (Read, Show)

data RNGInput = RNGInput
    { size          :: Int
    , encoding      :: Encoding
    } deriving (Show)
