module AESCipher.Types
(
  KeySize(..)
, AESCipherInput(..)
, CipherOperation(..)
, Input(..)
, Output(..)
) where

import qualified Data.Text as T

data Input = Stdin | InputFile FilePath
    deriving (Read, Show)

data Output = Stdout | OutputFile FilePath
    deriving (Read, Show)

data KeySize = KeySize128 | KeySize192 | KeySize256
    deriving (Read, Show)

data CipherOperation = Encrypt | Decrypt
    deriving (Read, Show)

data AESCipherInput = AESCipherInput
    { operation     :: CipherOperation
    , keySize       :: KeySize
    , key           :: T.Text
    , input         :: Input
    , output        :: Output
    }
    deriving (Read, Show)
