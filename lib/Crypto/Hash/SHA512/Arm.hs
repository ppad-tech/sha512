{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Crypto.Hash.SHA512.Arm
-- Copyright: (c) 2024 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- ARM crypto extension support for SHA-512.

module Crypto.Hash.SHA512.Arm (
    sha512_arm_available
  , hash_arm
  , hash_arm_with
  ) where

import Control.Monad (unless, when)
import qualified Data.Bits as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word8, Word64)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (poke, peek)
import Crypto.Hash.SHA512.Internal (unsafe_padding)
import System.IO.Unsafe (unsafePerformIO)

-- ffi -----------------------------------------------------------------------

foreign import ccall unsafe "sha512_block_arm"
  c_sha512_block :: Ptr Word64 -> Ptr Word8 -> IO ()

foreign import ccall unsafe "sha512_arm_available"
  c_sha512_arm_available :: IO Int

-- utilities -----------------------------------------------------------------

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- api -----------------------------------------------------------------------

sha512_arm_available :: Bool
sha512_arm_available = unsafePerformIO c_sha512_arm_available /= 0
{-# NOINLINE sha512_arm_available #-}

hash_arm :: BS.ByteString -> BS.ByteString
hash_arm = hash_arm_with mempty 0

-- | Hash with optional 128-byte prefix and extra length for padding.
hash_arm_with
  :: BS.ByteString  -- ^ optional 128-byte prefix (or empty)
  -> Word64         -- ^ extra length to add for padding
  -> BS.ByteString  -- ^ message
  -> BS.ByteString
hash_arm_with prefix el m@(BI.PS fp off l) = unsafePerformIO $
    allocaBytes 64 $ \state -> do
      poke_iv state
      -- process prefix block if provided
      unless (BS.null prefix) $ do
        let BI.PS pfp poff _ = prefix
        BI.unsafeWithForeignPtr pfp $ \src ->
          c_sha512_block state (src `plusPtr` poff)

      go state 0

      let !remaining@(BI.PS _ _ rlen) = BU.unsafeDrop (l - l `rem` 128) m
          BI.PS padfp padoff _ = unsafe_padding remaining (el + fi l)
      BI.unsafeWithForeignPtr padfp $ \src -> do
        c_sha512_block state (src `plusPtr` padoff)
        when (rlen >= 112) $
          c_sha512_block state (src `plusPtr` (padoff + 128))

      read_state state
  where
    go !state !j
      | j + 128 <= l = do
          BI.unsafeWithForeignPtr fp $ \src ->
            c_sha512_block state (src `plusPtr` (off + j))
          go state (j + 128)
      | otherwise = pure ()

-- arm helpers ---------------------------------------------------------------

poke_iv :: Ptr Word64 -> IO ()
poke_iv !state = do
  poke state                (0x6a09e667f3bcc908 :: Word64)
  poke (state `plusPtr` 8)  (0xbb67ae8584caa73b :: Word64)
  poke (state `plusPtr` 16) (0x3c6ef372fe94f82b :: Word64)
  poke (state `plusPtr` 24) (0xa54ff53a5f1d36f1 :: Word64)
  poke (state `plusPtr` 32) (0x510e527fade682d1 :: Word64)
  poke (state `plusPtr` 40) (0x9b05688c2b3e6c1f :: Word64)
  poke (state `plusPtr` 48) (0x1f83d9abfb41bd6b :: Word64)
  poke (state `plusPtr` 56) (0x5be0cd19137e2179 :: Word64)

read_state :: Ptr Word64 -> IO BS.ByteString
read_state !state = BI.create 64 $ \out -> do
  h0 <- peek state                :: IO Word64
  h1 <- peek (state `plusPtr` 8)  :: IO Word64
  h2 <- peek (state `plusPtr` 16) :: IO Word64
  h3 <- peek (state `plusPtr` 24) :: IO Word64
  h4 <- peek (state `plusPtr` 32) :: IO Word64
  h5 <- peek (state `plusPtr` 40) :: IO Word64
  h6 <- peek (state `plusPtr` 48) :: IO Word64
  h7 <- peek (state `plusPtr` 56) :: IO Word64
  poke_word64be out 0 h0
  poke_word64be out 8 h1
  poke_word64be out 16 h2
  poke_word64be out 24 h3
  poke_word64be out 32 h4
  poke_word64be out 40 h5
  poke_word64be out 48 h6
  poke_word64be out 56 h7

poke_word64be :: Ptr Word8 -> Int -> Word64 -> IO ()
poke_word64be !p !off !w = do
  poke (p `plusPtr` off)       (fi (w `B.unsafeShiftR` 56) :: Word8)
  poke (p `plusPtr` (off + 1)) (fi (w `B.unsafeShiftR` 48) :: Word8)
  poke (p `plusPtr` (off + 2)) (fi (w `B.unsafeShiftR` 40) :: Word8)
  poke (p `plusPtr` (off + 3)) (fi (w `B.unsafeShiftR` 32) :: Word8)
  poke (p `plusPtr` (off + 4)) (fi (w `B.unsafeShiftR` 24) :: Word8)
  poke (p `plusPtr` (off + 5)) (fi (w `B.unsafeShiftR` 16) :: Word8)
  poke (p `plusPtr` (off + 6)) (fi (w `B.unsafeShiftR` 8) :: Word8)
  poke (p `plusPtr` (off + 7)) (fi w :: Word8)
