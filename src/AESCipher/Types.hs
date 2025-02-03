module AESCipher.Types
(
  KeySize(..)
, AESCipherInput(..)
, CipherOperation(..)
) where

import IO.Types (Input, Output)
import qualified Data.Text as T

data KeySize = KeySize128 | KeySize192 | KeySize256
    deriving (Read, Show)

data CipherOperation = Encrypt | Decrypt
    deriving (Read, Show)

data AESCipherInput = AESCipherInput
    { operation     :: CipherOperation
    , keySize       :: KeySize
    , key           :: T.Text
    , nonce         :: T.Text
    , input         :: Input
    , output        :: Output
    }
    deriving (Read, Show)
