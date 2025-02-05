module Hash.Types
(
  HashAlgorithm(..)
, HashInput(..)
, OutputSize(..)
, Encoding(..)
) where

import IO.Types (Input, Output)

data HashAlgorithm  = SHA2 | SHA3
    deriving (Read, Show)

data OutputSize = Size224 | Size256 | Size384 | Size512
    deriving (Read, Enum, Bounded)

data Encoding = Hex | Base64
    deriving (Read, Show)

data HashInput = HashInput
    { hashAlgorithm     :: HashAlgorithm
    , outputSize        :: OutputSize
    , input             :: Input
    , output            :: Output
    , encoding          :: Encoding
    } deriving (Read, Show)

instance Show  OutputSize where
    show Size224 = "224"
    show Size256 = "256"
    show Size384 = "384"
    show Size512 = "512"
