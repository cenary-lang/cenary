module Cenary.Crypto.Keccak where

import           Crypto.Hash (Digest, Keccak_256, hash)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Monoid ((<>))
import           Text.Read (readMaybe)

keccak256 :: (Read b, Num b) => String -> Maybe b
keccak256 = readMaybe
          . ("0x" <>)
          . take 8
          . show
          . hash'
          . BS8.pack
  where
    -- | Let's help out the compiler a little
    hash' :: BS.ByteString -> Digest Keccak_256
    hash' = hash
