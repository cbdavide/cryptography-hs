module Hash.Handler
(
  processHash
) where

import Hash.Types (HashInput(..), HashAlgorithm(..), OutputSize(..))
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import IO.FileSystem (readInput, writeOutput)
import qualified Botan.Hash as Botan

processHash :: HashInput -> ExceptT String IO ()
processHash params = do
    input' <- liftIO $ readInput (input params)
    let hashFunction = getHashFunction (hashAlgorithm params) (outputSize params)
        output' = Botan.hash hashFunction input'

    liftIO $ writeOutput (output params) output'

getHashFunction :: HashAlgorithm -> OutputSize -> Botan.Hash
getHashFunction SHA2 Size224 = Botan.sha2_224
getHashFunction SHA2 Size256 = Botan.sha2_256
getHashFunction SHA2 Size384 = Botan.sha2_384
getHashFunction SHA2 Size512 = Botan.sha2_512
getHashFunction SHA3 Size224 = Botan.sha3_224
getHashFunction SHA3 Size256 = Botan.sha3_256
getHashFunction SHA3 Size384 = Botan.sha3_384
getHashFunction SHA3 Size512 = Botan.sha3_512
