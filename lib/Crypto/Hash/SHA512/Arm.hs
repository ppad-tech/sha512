{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module: Crypto.Hash.SHA512.Arm
-- Copyright: (c) 2024 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- ARM crypto extension support for SHA-512.

module Crypto.Hash.SHA512.Arm (
    sha512_arm_available
  , hash
  , hmac
  , _hmac_rr
  , _hmac_rsb
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word8, Word64)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr)
import qualified GHC.Exts as Exts
import qualified GHC.IO (IO(..))
import qualified GHC.Ptr
import Crypto.Hash.SHA512.Internal hiding (update)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- ffi ------------------------------------------------------------------------

foreign import ccall unsafe "sha512_block_arm"
  c_sha512_block :: Ptr Word64 -> Ptr Word64 -> IO ()

foreign import ccall unsafe "sha512_arm_available"
  c_sha512_arm_available :: IO Int

-- utilities ------------------------------------------------------------------

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

peek_registers
  :: Ptr Word64
  -> Registers
peek_registers (GHC.Ptr.Ptr addr) = R
  (Exts.indexWord64OffAddr# addr 0#)
  (Exts.indexWord64OffAddr# addr 1#)
  (Exts.indexWord64OffAddr# addr 2#)
  (Exts.indexWord64OffAddr# addr 3#)
  (Exts.indexWord64OffAddr# addr 4#)
  (Exts.indexWord64OffAddr# addr 5#)
  (Exts.indexWord64OffAddr# addr 6#)
  (Exts.indexWord64OffAddr# addr 7#)
{-# INLINE peek_registers #-}

poke_block :: Ptr Word64 -> Block -> IO ()
poke_block
    (GHC.Ptr.Ptr addr)
    (B w00 w01 w02 w03 w04 w05 w06 w07 w08 w09 w10 w11 w12 w13 w14 w15)
  = GHC.IO.IO $ \s00 ->
      case Exts.writeWord64OffAddr# addr 00# w00 s00 of { s01 ->
      case Exts.writeWord64OffAddr# addr 01# w01 s01 of { s02 ->
      case Exts.writeWord64OffAddr# addr 02# w02 s02 of { s03 ->
      case Exts.writeWord64OffAddr# addr 03# w03 s03 of { s04 ->
      case Exts.writeWord64OffAddr# addr 04# w04 s04 of { s05 ->
      case Exts.writeWord64OffAddr# addr 05# w05 s05 of { s06 ->
      case Exts.writeWord64OffAddr# addr 06# w06 s06 of { s07 ->
      case Exts.writeWord64OffAddr# addr 07# w07 s07 of { s08 ->
      case Exts.writeWord64OffAddr# addr 08# w08 s08 of { s09 ->
      case Exts.writeWord64OffAddr# addr 09# w09 s09 of { s10 ->
      case Exts.writeWord64OffAddr# addr 10# w10 s10 of { s11 ->
      case Exts.writeWord64OffAddr# addr 11# w11 s11 of { s12 ->
      case Exts.writeWord64OffAddr# addr 12# w12 s12 of { s13 ->
      case Exts.writeWord64OffAddr# addr 13# w13 s13 of { s14 ->
      case Exts.writeWord64OffAddr# addr 14# w14 s14 of { s15 ->
      case Exts.writeWord64OffAddr# addr 15# w15 s15 of { s16 ->
      (# s16, () #) }}}}}}}}}}}}}}}}
{-# INLINE poke_block #-}

-- update ---------------------------------------------------------------------

update :: Ptr Word64 -> Ptr Word64 -> Block -> IO ()
update rp bp block = do
  poke_block bp block
  c_sha512_block rp bp
{-# INLINE update #-}

-- api -----------------------------------------------------------------------

-- | Are ARM +sha512 extensions available?
sha512_arm_available :: Bool
sha512_arm_available = unsafeDupablePerformIO c_sha512_arm_available /= 0
{-# NOINLINE sha512_arm_available #-}

hash
  :: BS.ByteString
  -> BS.ByteString
hash m = unsafeDupablePerformIO $
  allocaBytes 64 $ \rp ->
  allocaBytes 128 $ \bp -> do
    poke_registers rp (iv ())
    _hash rp bp 0 m
    let !rs = peek_registers rp
    pure (cat rs)

_hash
  :: Ptr Word64    -- ^ register state
  -> Ptr Word64    -- ^ block state
  -> Word64        -- ^ extra prefix length, for padding calculation
  -> BS.ByteString -- ^ input
  -> IO ()
_hash rp bp el m@(BI.PS _ _ l) = do
  hash_blocks rp bp m
  let !fin@(BI.PS _ _ ll) = BU.unsafeDrop (l - l `rem` 128) m
      !total = el + fi l
  if   ll < 112
  then do
    let !ult = parse_pad1 fin total
    update rp bp ult
  else do
    let !(# pen, ult #) = parse_pad2 fin total
    update rp bp pen
    update rp bp ult
{-# INLINABLE _hash #-}

hash_blocks
  :: Ptr Word64    -- ^ register state
  -> Ptr Word64    -- ^ block state
  -> BS.ByteString -- ^ input
  -> IO ()
hash_blocks rp bp m@(BI.PS _ _ l) = loop 0 where
  loop !j
    | j + 128 > l = pure ()
    | otherwise   = do
        let !block = parse m j
        update rp bp block
        loop (j + 128)
{-# INLINE hash_blocks #-}

-- hmac -----------------------------------------------------------------------

hmac :: BS.ByteString -> BS.ByteString -> BS.ByteString
hmac k m = unsafeDupablePerformIO $
  allocaBytes 64 $ \rp ->
  allocaBytes 128 $ \bp -> do
    _hmac rp bp (prep_key k) m
    pure (cat (peek_registers rp))

prep_key :: BS.ByteString -> Block
prep_key k@(BI.PS _ _ l)
    | l > 128   = parse_key (hash k)
    | otherwise = parse_key k
{-# INLINABLE prep_key #-}

-- assume padded key as block.
_hmac
  :: Ptr Word64    -- ^ register state
  -> Ptr Word64    -- ^ block state
  -> Block         -- ^ padded key
  -> BS.ByteString -- ^ message
  -> IO ()
_hmac rp bp k m = do
  poke_registers rp (iv ())
  update rp bp (xor k (Exts.wordToWord64# 0x3636363636363636##))
  _hash rp bp 128 m
  let !block = pad_registers_with_length (peek_registers rp)
  poke_registers rp (iv ())
  update rp bp (xor k (Exts.wordToWord64# 0x5C5C5C5C5C5C5C5C##))
  update rp bp block
{-# NOINLINE _hmac #-}

_hmac_rr
  :: Ptr Word64 -- ^ register state
  -> Ptr Word64 -- ^ block state
  -> Registers  -- ^ key
  -> Registers  -- ^ message
  -> IO ()
_hmac_rr rp bp k m = do
  let !key   = pad_registers k
      !block = pad_registers_with_length m
  _hmac_bb rp bp key block
{-# INLINABLE _hmac_rr #-}

_hmac_bb
  :: Ptr Word64  -- ^ register state
  -> Ptr Word64  -- ^ block state
  -> Block       -- ^ padded key
  -> Block       -- ^ padded message
  -> IO ()
_hmac_bb rp bp k m = do
  poke_registers rp (iv ())
  update rp bp (xor k (Exts.wordToWord64# 0x3636363636363636##))
  update rp bp m
  let !inner = pad_registers_with_length (peek_registers rp)
  poke_registers rp (iv ())
  update rp bp (xor k (Exts.wordToWord64# 0x5C5C5C5C5C5C5C5C##))
  update rp bp inner
{-# INLINABLE _hmac_bb #-}

-- | HMAC(key, v || sep || data) using ARM crypto extensions.
-- Writes result to destination pointer.
_hmac_rsb
  :: Ptr Word64    -- ^ destination (8 Word64s)
  -> Ptr Word64    -- ^ scratch block buffer (16 Word64s)
  -> Registers     -- ^ key
  -> Registers     -- ^ v
  -> Word8         -- ^ separator byte
  -> BS.ByteString -- ^ data
  -> IO ()
_hmac_rsb rp bp k v sep dat = do
  poke_registers rp (iv ())
  let !key = pad_registers k
  update rp bp (xor key (Exts.wordToWord64# 0x3636363636363636##))
  _hash_vsb rp bp 128 v sep dat
  let !inner = pad_registers_with_length (peek_registers rp)
  poke_registers rp (iv ())
  update rp bp (xor key (Exts.wordToWord64# 0x5C5C5C5C5C5C5C5C##))
  update rp bp inner
{-# INLINABLE _hmac_rsb #-}

-- | Hash (v || sep || dat) with ARM crypto extensions.
-- Assumes register state already initialized at rp.
_hash_vsb
  :: Ptr Word64    -- ^ register state
  -> Ptr Word64    -- ^ block buffer
  -> Word64        -- ^ extra prefix length
  -> Registers     -- ^ v
  -> Word8         -- ^ sep
  -> BS.ByteString -- ^ dat
  -> IO ()
_hash_vsb rp bp el v sep dat@(BI.PS _ _ l)
  | l >= 63 = do
      -- first block is complete: v || sep || dat[0:63]
      let !b0 = parse_vsb v sep dat
      update rp bp b0
      -- hash remaining complete blocks from dat[63:]
      let !rest    = BU.unsafeDrop 63 dat
          !restLen = l - 63
      hash_blocks rp bp rest
      -- handle final padding
      let !finLen = restLen `rem` 128
          !fin    = BU.unsafeDrop (restLen - finLen) rest
          !total  = el + 65 + fi l
      if   finLen < 112
      then update rp bp (parse_pad1 fin total)
      else do
        let !(# pen, ult #) = parse_pad2 fin total
        update rp bp pen
        update rp bp ult
  | otherwise = do
      -- message < 128 bytes total, straight to padding
      let !total = el + 65 + fi l
      if   65 + l < 112
      then update rp bp (parse_pad1_vsb v sep dat total)
      else do
        let !(# pen, ult #) = parse_pad2_vsb v sep dat total
        update rp bp pen
        update rp bp ult
{-# INLINABLE _hash_vsb #-}

