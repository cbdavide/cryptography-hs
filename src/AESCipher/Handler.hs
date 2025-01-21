module AESCipher.Handler
( processAESCipherHandler
) where

import AESCipher.Types (AESCipherInput (..), KeySize(..))
import Control.Monad (unless)
import Control.Monad.Except
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO(liftIO))

processAESCipherHandler :: AESCipherInput -> ExceptT String IO ()
processAESCipherHandler x = do
    _ <- validateKey x
    liftIO $ putStrLn "hey hey"

validateKey :: AESCipherInput -> ExceptT String IO ()
validateKey x = do
    let klen = T.length $ key x
        expectedKeyLength = case keySize x of
            KeySize128 -> 32
            KeySize192 -> 48
            KeySize256 -> 64

    unless (klen == expectedKeyLength) $ do
        throwError $ "Invalid key length: expected "
                  ++ show expectedKeyLength
                  ++ " hex characters, but got "
                  ++ show klen
