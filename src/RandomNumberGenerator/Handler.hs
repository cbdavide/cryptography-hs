{-# LANGUAGE OverloadedStrings #-}

module RandomNumberGenerator.Handler
(
   processRNGCommand
)
where

import Data.Char (intToDigit)
import Data.Word
import Botan.Utility (hexEncode, base64Encode, HexCase (Lower))
import Numeric (showIntAtBase)
import RandomNumberGenerator.Types (RNGInput(..), Encoding (..), OutputType(..))

import qualified Data.Binary as Binary
import qualified Botan.RNG as Botan
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BF

generateRandomBytes :: Int -> Botan.RandomIO BS.ByteString
generateRandomBytes num = do
    Botan.getRandomBytes num

generateRandomByteString :: Int -> IO BS.ByteString
generateRandomByteString n = do
    rng <- Botan.newRNG Botan.Autoseeded
    Botan.runRandomIO (generateRandomBytes n) rng

processRNGCommand :: RNGInput -> IO ()
processRNGCommand input = do
    case outputType input of
        Number -> generateNumber input
        ByteString -> generateByteString input

generateNumber :: RNGInput -> IO ()
generateNumber input = do
    number <- generateRandomByteString 8
    let decoded = Binary.decode $ BF.fromStrict number :: Word64

        outputNumber = if maxNumber input > 0
            then mapToInterval input decoded
            else decoded

    print outputNumber

mapToInterval :: RNGInput -> Word64 -> Word64
mapToInterval input value = result
    where low = fromIntegral (minNumber input)
          hi = fromIntegral (maxNumber input)
          range = hi - low + 1
          result = low + (value `rem` range)

generateByteString :: RNGInput -> IO ()
generateByteString input = do
    x <- generateRandomByteString (numberOfBytes input)
    putStrLn $ T.unpack (encodeByteString (encoding input) x)

encodeByteString :: Encoding -> BS.ByteString -> T.Text
encodeByteString Hex bs = hexEncode bs Lower
encodeByteString Base64 bs = base64Encode bs
encodeByteString UTF8 bs = TE.decodeUtf8Lenient bs
encodeByteString Binary bs = toBinaryStr bs

toBinaryStr :: BS.ByteString -> T.Text
toBinaryStr input = T.concat bytesStr
    where bytesStr = fmap encode (BS.unpack input)
          encode :: Word8 -> T.Text
          encode w = let bin = T.pack (showIntAtBase 2 intToDigit w "")
                     in T.replicate (8 - T.length bin) "0" <> bin
