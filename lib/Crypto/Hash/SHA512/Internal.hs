{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Crypto.Hash.SHA512.Internal
-- Copyright: (c) 2024 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- SHA-512 internals.

module Crypto.Hash.SHA512.Internal (
  -- * Types
    Block(B, ..)
  , Registers(R, ..)
  , MAC(..)

  -- * Parsing
  , parse
  , parse_pad1
  , parse_pad2

  -- * Serializing
  , cat
  , cat_into

  -- * Hash function internals
  , update
  , iv

  -- * HMAC utilities
  , pad_registers
  , pad_registers_with_length
  , xor
  , parse_key

  -- * HMAC-DRBG utilities
  , parse_vsb
  , parse_pad1_vsb
  , parse_pad2_vsb

  -- * Pointer-based IO utilities
  , poke_registers
  ) where

import qualified Data.Bits as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word8, Word64)
import qualified GHC.IO (IO(..))
import GHC.Ptr (Ptr(..))
import GHC.Exts (Int#)
import qualified GHC.Exts as Exts
import qualified GHC.Word (Word64(..), Word8(..))

-- types ----------------------------------------------------------------------

-- | A message authentication code.
--
--   Note that you should compare MACs for equality using the 'Eq'
--   instance, which performs the comparison in constant time, instead
--   of unwrapping and comparing the underlying 'ByteStrings'.
--
--   >>> let foo@(MAC bs0) = hmac key "hi"
--   >>> let bar@(MAC bs1) = hmac key "there"
--   >>> foo == bar -- do this
--   False
--   >>> bs0 == bs1 -- don't do this
--   False
newtype MAC = MAC BS.ByteString
  deriving newtype Show

instance Eq MAC where
  -- | A constant-time equality check for message authentication codes.
  --
  --   Runs in variable-time only for invalid inputs.
  (MAC a@(BI.PS _ _ la)) == (MAC b@(BI.PS _ _ lb))
    | la /= lb  = False
    | otherwise = BS.foldl' (B..|.) 0 (BS.packZipWith B.xor a b) == 0

-- | SHA512 block.
newtype Block = Block
  (# Exts.Word64#, Exts.Word64#, Exts.Word64#, Exts.Word64#
  ,  Exts.Word64#, Exts.Word64#, Exts.Word64#, Exts.Word64#
  ,  Exts.Word64#, Exts.Word64#, Exts.Word64#, Exts.Word64#
  ,  Exts.Word64#, Exts.Word64#, Exts.Word64#, Exts.Word64#
  #)

pattern B
  :: Exts.Word64# -> Exts.Word64# -> Exts.Word64# -> Exts.Word64#
  -> Exts.Word64# -> Exts.Word64# -> Exts.Word64# -> Exts.Word64#
  -> Exts.Word64# -> Exts.Word64# -> Exts.Word64# -> Exts.Word64#
  -> Exts.Word64# -> Exts.Word64# -> Exts.Word64# -> Exts.Word64#
  -> Block
pattern B w00 w01 w02 w03 w04 w05 w06 w07 w08 w09 w10 w11 w12 w13 w14 w15 =
  Block
    (# w00, w01, w02, w03, w04, w05, w06, w07
    ,  w08, w09, w10, w11, w12, w13, w14, w15
    #)
{-# COMPLETE B #-}

-- | SHA512 state.
newtype Registers = Registers
  (# Exts.Word64#, Exts.Word64#, Exts.Word64#, Exts.Word64#
  ,  Exts.Word64#, Exts.Word64#, Exts.Word64#, Exts.Word64#
  #)

pattern R
  :: Exts.Word64# -> Exts.Word64# -> Exts.Word64# -> Exts.Word64#
  -> Exts.Word64# -> Exts.Word64# -> Exts.Word64# -> Exts.Word64#
  -> Registers
pattern R w00 w01 w02 w03 w04 w05 w06 w07 = Registers
  (# w00, w01, w02, w03
  ,  w04, w05, w06, w07
  #)
{-# COMPLETE R #-}

-- parsing (nonfinal input) ---------------------------------------------------

-- | Given a bytestring and offset, parse a full block.
--
--   The length of the input is not checked.
parse :: BS.ByteString -> Int -> Block
parse bs m = B
  (word64be bs m)
  (word64be bs (m + 08))
  (word64be bs (m + 16))
  (word64be bs (m + 24))
  (word64be bs (m + 32))
  (word64be bs (m + 40))
  (word64be bs (m + 48))
  (word64be bs (m + 56))
  (word64be bs (m + 64))
  (word64be bs (m + 72))
  (word64be bs (m + 80))
  (word64be bs (m + 88))
  (word64be bs (m + 96))
  (word64be bs (m + 104))
  (word64be bs (m + 112))
  (word64be bs (m + 120))
{-# INLINE parse #-}

-- | Parse the 64-bit word encoded at the given offset.
--
--   The length of the input is not checked.
word64be :: BS.ByteString -> Int -> Exts.Word64#
word64be bs m =
  let !(GHC.Word.W8# r0) = BU.unsafeIndex bs m
      !(GHC.Word.W8# r1) = BU.unsafeIndex bs (m + 1)
      !(GHC.Word.W8# r2) = BU.unsafeIndex bs (m + 2)
      !(GHC.Word.W8# r3) = BU.unsafeIndex bs (m + 3)
      !(GHC.Word.W8# r4) = BU.unsafeIndex bs (m + 4)
      !(GHC.Word.W8# r5) = BU.unsafeIndex bs (m + 5)
      !(GHC.Word.W8# r6) = BU.unsafeIndex bs (m + 6)
      !(GHC.Word.W8# r7) = BU.unsafeIndex bs (m + 7)
      !w0 = Exts.word8ToWord# r0
      !w1 = Exts.word8ToWord# r1
      !w2 = Exts.word8ToWord# r2
      !w3 = Exts.word8ToWord# r3
      !w4 = Exts.word8ToWord# r4
      !w5 = Exts.word8ToWord# r5
      !w6 = Exts.word8ToWord# r6
      !w7 = Exts.word8ToWord# r7
      !s0 = Exts.uncheckedShiftL# w0 56#
      !s1 = Exts.uncheckedShiftL# w1 48#
      !s2 = Exts.uncheckedShiftL# w2 40#
      !s3 = Exts.uncheckedShiftL# w3 32#
      !s4 = Exts.uncheckedShiftL# w4 24#
      !s5 = Exts.uncheckedShiftL# w5 16#
      !s6 = Exts.uncheckedShiftL# w6 8#
  in  Exts.wordToWord64#
        (s0 `Exts.or#` s1 `Exts.or#` s2 `Exts.or#` s3 `Exts.or#`
         s4 `Exts.or#` s5 `Exts.or#` s6 `Exts.or#` w7)
{-# INLINE word64be #-}

-- parsing (final input) ------------------------------------------------------

-- | Parse the final chunk of an input message, assuming it is less than
--   112 bytes in length (unchecked!).
--
--   Returns one block consisting of the chunk and padding.
parse_pad1
  :: BS.ByteString -- ^ final input chunk (< 112 bytes)
  -> Word64        -- ^ length of all input
  -> Block         -- ^ resulting block
parse_pad1 bs l =
  let !bits = l * 8
      !(GHC.Word.W64# llo) = bits
  in  B (w64_at bs 000) (w64_at bs 008) (w64_at bs 016) (w64_at bs 024)
        (w64_at bs 032) (w64_at bs 040) (w64_at bs 048) (w64_at bs 056)
        (w64_at bs 064) (w64_at bs 072) (w64_at bs 080) (w64_at bs 088)
        (w64_at bs 096) (w64_at bs 104) (Exts.wordToWord64# 0##) llo
{-# INLINABLE parse_pad1 #-}

-- | Parse the final chunk of an input message, assuming it is at least 112
--   bytes in length (unchecked!).
--
--   Returns two blocks consisting of the chunk and padding.
parse_pad2
  :: BS.ByteString       -- ^ final input chunk (>= 112 bytes)
  -> Word64              -- ^ length of all input
  -> (# Block, Block #)  -- ^ resulting blocks
parse_pad2 bs l =
  let !bits = l * 8
      !z    = Exts.wordToWord64# 0##
      !(GHC.Word.W64# llo) = bits
      !block0 = B
        (w64_at bs 000) (w64_at bs 008) (w64_at bs 016) (w64_at bs 024)
        (w64_at bs 032) (w64_at bs 040) (w64_at bs 048) (w64_at bs 056)
        (w64_at bs 064) (w64_at bs 072) (w64_at bs 080) (w64_at bs 088)
        (w64_at bs 096) (w64_at bs 104) (w64_at bs 112) (w64_at bs 120)
      !block1 = B z z z z z z z z z z z z z z z llo
  in  (# block0, block1 #)
{-# INLINABLE parse_pad2 #-}

-- | Return the byte at offset 'i', or a padding separator or zero byte
--   beyond the input bounds, as an unboxed word.
w8_as_w64_at
  :: BS.ByteString  -- ^ input chunk
  -> Int            -- ^ offset
  -> Exts.Word#
w8_as_w64_at bs@(BI.PS _ _ l) i = case compare i l of
  LT -> let !(GHC.Word.W8# w) = BU.unsafeIndex bs i
        in  Exts.word8ToWord# w
  EQ -> 0x80##
  _  -> 0x00##
{-# INLINE w8_as_w64_at #-}

-- | Return the 64-bit word encoded by eight consecutive bytes at the
--   provided offset.
w64_at
  :: BS.ByteString
  -> Int
  -> Exts.Word64#
w64_at bs i =
  let !w0 = w8_as_w64_at bs i       `Exts.uncheckedShiftL#` 56#
      !w1 = w8_as_w64_at bs (i + 1) `Exts.uncheckedShiftL#` 48#
      !w2 = w8_as_w64_at bs (i + 2) `Exts.uncheckedShiftL#` 40#
      !w3 = w8_as_w64_at bs (i + 3) `Exts.uncheckedShiftL#` 32#
      !w4 = w8_as_w64_at bs (i + 4) `Exts.uncheckedShiftL#` 24#
      !w5 = w8_as_w64_at bs (i + 5) `Exts.uncheckedShiftL#` 16#
      !w6 = w8_as_w64_at bs (i + 6) `Exts.uncheckedShiftL#` 08#
      !w7 = w8_as_w64_at bs (i + 7)
  in  Exts.wordToWord64#
        (w0 `Exts.or#` w1 `Exts.or#` w2 `Exts.or#` w3 `Exts.or#`
         w4 `Exts.or#` w5 `Exts.or#` w6 `Exts.or#` w7)
{-# INLINE w64_at #-}

-- update ---------------------------------------------------------------------

-- | Update register state, given new input block.
update :: Registers -> Block -> Registers
update
    (R h0 h1 h2 h3 h4 h5 h6 h7)
    (B b00 b01 b02 b03 b04 b05 b06 b07 b08 b09 b10 b11 b12 b13 b14 b15)
  =
  let -- message schedule
      !w00 = b00; !w01 = b01; !w02 = b02; !w03 = b03
      !w04 = b04; !w05 = b05; !w06 = b06; !w07 = b07
      !w08 = b08; !w09 = b09; !w10 = b10; !w11 = b11
      !w12 = b12; !w13 = b13; !w14 = b14; !w15 = b15
      !w16 = ssig1# w14 `p` w09 `p` ssig0# w01 `p` w00
      !w17 = ssig1# w15 `p` w10 `p` ssig0# w02 `p` w01
      !w18 = ssig1# w16 `p` w11 `p` ssig0# w03 `p` w02
      !w19 = ssig1# w17 `p` w12 `p` ssig0# w04 `p` w03
      !w20 = ssig1# w18 `p` w13 `p` ssig0# w05 `p` w04
      !w21 = ssig1# w19 `p` w14 `p` ssig0# w06 `p` w05
      !w22 = ssig1# w20 `p` w15 `p` ssig0# w07 `p` w06
      !w23 = ssig1# w21 `p` w16 `p` ssig0# w08 `p` w07
      !w24 = ssig1# w22 `p` w17 `p` ssig0# w09 `p` w08
      !w25 = ssig1# w23 `p` w18 `p` ssig0# w10 `p` w09
      !w26 = ssig1# w24 `p` w19 `p` ssig0# w11 `p` w10
      !w27 = ssig1# w25 `p` w20 `p` ssig0# w12 `p` w11
      !w28 = ssig1# w26 `p` w21 `p` ssig0# w13 `p` w12
      !w29 = ssig1# w27 `p` w22 `p` ssig0# w14 `p` w13
      !w30 = ssig1# w28 `p` w23 `p` ssig0# w15 `p` w14
      !w31 = ssig1# w29 `p` w24 `p` ssig0# w16 `p` w15
      !w32 = ssig1# w30 `p` w25 `p` ssig0# w17 `p` w16
      !w33 = ssig1# w31 `p` w26 `p` ssig0# w18 `p` w17
      !w34 = ssig1# w32 `p` w27 `p` ssig0# w19 `p` w18
      !w35 = ssig1# w33 `p` w28 `p` ssig0# w20 `p` w19
      !w36 = ssig1# w34 `p` w29 `p` ssig0# w21 `p` w20
      !w37 = ssig1# w35 `p` w30 `p` ssig0# w22 `p` w21
      !w38 = ssig1# w36 `p` w31 `p` ssig0# w23 `p` w22
      !w39 = ssig1# w37 `p` w32 `p` ssig0# w24 `p` w23
      !w40 = ssig1# w38 `p` w33 `p` ssig0# w25 `p` w24
      !w41 = ssig1# w39 `p` w34 `p` ssig0# w26 `p` w25
      !w42 = ssig1# w40 `p` w35 `p` ssig0# w27 `p` w26
      !w43 = ssig1# w41 `p` w36 `p` ssig0# w28 `p` w27
      !w44 = ssig1# w42 `p` w37 `p` ssig0# w29 `p` w28
      !w45 = ssig1# w43 `p` w38 `p` ssig0# w30 `p` w29
      !w46 = ssig1# w44 `p` w39 `p` ssig0# w31 `p` w30
      !w47 = ssig1# w45 `p` w40 `p` ssig0# w32 `p` w31
      !w48 = ssig1# w46 `p` w41 `p` ssig0# w33 `p` w32
      !w49 = ssig1# w47 `p` w42 `p` ssig0# w34 `p` w33
      !w50 = ssig1# w48 `p` w43 `p` ssig0# w35 `p` w34
      !w51 = ssig1# w49 `p` w44 `p` ssig0# w36 `p` w35
      !w52 = ssig1# w50 `p` w45 `p` ssig0# w37 `p` w36
      !w53 = ssig1# w51 `p` w46 `p` ssig0# w38 `p` w37
      !w54 = ssig1# w52 `p` w47 `p` ssig0# w39 `p` w38
      !w55 = ssig1# w53 `p` w48 `p` ssig0# w40 `p` w39
      !w56 = ssig1# w54 `p` w49 `p` ssig0# w41 `p` w40
      !w57 = ssig1# w55 `p` w50 `p` ssig0# w42 `p` w41
      !w58 = ssig1# w56 `p` w51 `p` ssig0# w43 `p` w42
      !w59 = ssig1# w57 `p` w52 `p` ssig0# w44 `p` w43
      !w60 = ssig1# w58 `p` w53 `p` ssig0# w45 `p` w44
      !w61 = ssig1# w59 `p` w54 `p` ssig0# w46 `p` w45
      !w62 = ssig1# w60 `p` w55 `p` ssig0# w47 `p` w46
      !w63 = ssig1# w61 `p` w56 `p` ssig0# w48 `p` w47
      !w64 = ssig1# w62 `p` w57 `p` ssig0# w49 `p` w48
      !w65 = ssig1# w63 `p` w58 `p` ssig0# w50 `p` w49
      !w66 = ssig1# w64 `p` w59 `p` ssig0# w51 `p` w50
      !w67 = ssig1# w65 `p` w60 `p` ssig0# w52 `p` w51
      !w68 = ssig1# w66 `p` w61 `p` ssig0# w53 `p` w52
      !w69 = ssig1# w67 `p` w62 `p` ssig0# w54 `p` w53
      !w70 = ssig1# w68 `p` w63 `p` ssig0# w55 `p` w54
      !w71 = ssig1# w69 `p` w64 `p` ssig0# w56 `p` w55
      !w72 = ssig1# w70 `p` w65 `p` ssig0# w57 `p` w56
      !w73 = ssig1# w71 `p` w66 `p` ssig0# w58 `p` w57
      !w74 = ssig1# w72 `p` w67 `p` ssig0# w59 `p` w58
      !w75 = ssig1# w73 `p` w68 `p` ssig0# w60 `p` w59
      !w76 = ssig1# w74 `p` w69 `p` ssig0# w61 `p` w60
      !w77 = ssig1# w75 `p` w70 `p` ssig0# w62 `p` w61
      !w78 = ssig1# w76 `p` w71 `p` ssig0# w63 `p` w62
      !w79 = ssig1# w77 `p` w72 `p` ssig0# w64 `p` w63

      -- rounds (constants are cube roots of first 80 primes)
      !(R s00a s00b s00c s00d s00e s00f s00g s00h) =
        step# h0 h1 h2 h3 h4 h5 h6 h7 (k 0x428a2f98d728ae22##) w00
      !(R s01a s01b s01c s01d s01e s01f s01g s01h) =
        step# s00a s00b s00c s00d s00e s00f s00g s00h
          (k 0x7137449123ef65cd##) w01
      !(R s02a s02b s02c s02d s02e s02f s02g s02h) =
        step# s01a s01b s01c s01d s01e s01f s01g s01h
          (k 0xb5c0fbcfec4d3b2f##) w02
      !(R s03a s03b s03c s03d s03e s03f s03g s03h) =
        step# s02a s02b s02c s02d s02e s02f s02g s02h
          (k 0xe9b5dba58189dbbc##) w03
      !(R s04a s04b s04c s04d s04e s04f s04g s04h) =
        step# s03a s03b s03c s03d s03e s03f s03g s03h
          (k 0x3956c25bf348b538##) w04
      !(R s05a s05b s05c s05d s05e s05f s05g s05h) =
        step# s04a s04b s04c s04d s04e s04f s04g s04h
          (k 0x59f111f1b605d019##) w05
      !(R s06a s06b s06c s06d s06e s06f s06g s06h) =
        step# s05a s05b s05c s05d s05e s05f s05g s05h
          (k 0x923f82a4af194f9b##) w06
      !(R s07a s07b s07c s07d s07e s07f s07g s07h) =
        step# s06a s06b s06c s06d s06e s06f s06g s06h
          (k 0xab1c5ed5da6d8118##) w07
      !(R s08a s08b s08c s08d s08e s08f s08g s08h) =
        step# s07a s07b s07c s07d s07e s07f s07g s07h
          (k 0xd807aa98a3030242##) w08
      !(R s09a s09b s09c s09d s09e s09f s09g s09h) =
        step# s08a s08b s08c s08d s08e s08f s08g s08h
          (k 0x12835b0145706fbe##) w09
      !(R s10a s10b s10c s10d s10e s10f s10g s10h) =
        step# s09a s09b s09c s09d s09e s09f s09g s09h
          (k 0x243185be4ee4b28c##) w10
      !(R s11a s11b s11c s11d s11e s11f s11g s11h) =
        step# s10a s10b s10c s10d s10e s10f s10g s10h
          (k 0x550c7dc3d5ffb4e2##) w11
      !(R s12a s12b s12c s12d s12e s12f s12g s12h) =
        step# s11a s11b s11c s11d s11e s11f s11g s11h
          (k 0x72be5d74f27b896f##) w12
      !(R s13a s13b s13c s13d s13e s13f s13g s13h) =
        step# s12a s12b s12c s12d s12e s12f s12g s12h
          (k 0x80deb1fe3b1696b1##) w13
      !(R s14a s14b s14c s14d s14e s14f s14g s14h) =
        step# s13a s13b s13c s13d s13e s13f s13g s13h
          (k 0x9bdc06a725c71235##) w14
      !(R s15a s15b s15c s15d s15e s15f s15g s15h) =
        step# s14a s14b s14c s14d s14e s14f s14g s14h
          (k 0xc19bf174cf692694##) w15
      !(R s16a s16b s16c s16d s16e s16f s16g s16h) =
        step# s15a s15b s15c s15d s15e s15f s15g s15h
          (k 0xe49b69c19ef14ad2##) w16
      !(R s17a s17b s17c s17d s17e s17f s17g s17h) =
        step# s16a s16b s16c s16d s16e s16f s16g s16h
          (k 0xefbe4786384f25e3##) w17
      !(R s18a s18b s18c s18d s18e s18f s18g s18h) =
        step# s17a s17b s17c s17d s17e s17f s17g s17h
          (k 0x0fc19dc68b8cd5b5##) w18
      !(R s19a s19b s19c s19d s19e s19f s19g s19h) =
        step# s18a s18b s18c s18d s18e s18f s18g s18h
          (k 0x240ca1cc77ac9c65##) w19
      !(R s20a s20b s20c s20d s20e s20f s20g s20h) =
        step# s19a s19b s19c s19d s19e s19f s19g s19h
          (k 0x2de92c6f592b0275##) w20
      !(R s21a s21b s21c s21d s21e s21f s21g s21h) =
        step# s20a s20b s20c s20d s20e s20f s20g s20h
          (k 0x4a7484aa6ea6e483##) w21
      !(R s22a s22b s22c s22d s22e s22f s22g s22h) =
        step# s21a s21b s21c s21d s21e s21f s21g s21h
          (k 0x5cb0a9dcbd41fbd4##) w22
      !(R s23a s23b s23c s23d s23e s23f s23g s23h) =
        step# s22a s22b s22c s22d s22e s22f s22g s22h
          (k 0x76f988da831153b5##) w23
      !(R s24a s24b s24c s24d s24e s24f s24g s24h) =
        step# s23a s23b s23c s23d s23e s23f s23g s23h
          (k 0x983e5152ee66dfab##) w24
      !(R s25a s25b s25c s25d s25e s25f s25g s25h) =
        step# s24a s24b s24c s24d s24e s24f s24g s24h
          (k 0xa831c66d2db43210##) w25
      !(R s26a s26b s26c s26d s26e s26f s26g s26h) =
        step# s25a s25b s25c s25d s25e s25f s25g s25h
          (k 0xb00327c898fb213f##) w26
      !(R s27a s27b s27c s27d s27e s27f s27g s27h) =
        step# s26a s26b s26c s26d s26e s26f s26g s26h
          (k 0xbf597fc7beef0ee4##) w27
      !(R s28a s28b s28c s28d s28e s28f s28g s28h) =
        step# s27a s27b s27c s27d s27e s27f s27g s27h
          (k 0xc6e00bf33da88fc2##) w28
      !(R s29a s29b s29c s29d s29e s29f s29g s29h) =
        step# s28a s28b s28c s28d s28e s28f s28g s28h
          (k 0xd5a79147930aa725##) w29
      !(R s30a s30b s30c s30d s30e s30f s30g s30h) =
        step# s29a s29b s29c s29d s29e s29f s29g s29h
          (k 0x06ca6351e003826f##) w30
      !(R s31a s31b s31c s31d s31e s31f s31g s31h) =
        step# s30a s30b s30c s30d s30e s30f s30g s30h
          (k 0x142929670a0e6e70##) w31
      !(R s32a s32b s32c s32d s32e s32f s32g s32h) =
        step# s31a s31b s31c s31d s31e s31f s31g s31h
          (k 0x27b70a8546d22ffc##) w32
      !(R s33a s33b s33c s33d s33e s33f s33g s33h) =
        step# s32a s32b s32c s32d s32e s32f s32g s32h
          (k 0x2e1b21385c26c926##) w33
      !(R s34a s34b s34c s34d s34e s34f s34g s34h) =
        step# s33a s33b s33c s33d s33e s33f s33g s33h
          (k 0x4d2c6dfc5ac42aed##) w34
      !(R s35a s35b s35c s35d s35e s35f s35g s35h) =
        step# s34a s34b s34c s34d s34e s34f s34g s34h
          (k 0x53380d139d95b3df##) w35
      !(R s36a s36b s36c s36d s36e s36f s36g s36h) =
        step# s35a s35b s35c s35d s35e s35f s35g s35h
          (k 0x650a73548baf63de##) w36
      !(R s37a s37b s37c s37d s37e s37f s37g s37h) =
        step# s36a s36b s36c s36d s36e s36f s36g s36h
          (k 0x766a0abb3c77b2a8##) w37
      !(R s38a s38b s38c s38d s38e s38f s38g s38h) =
        step# s37a s37b s37c s37d s37e s37f s37g s37h
          (k 0x81c2c92e47edaee6##) w38
      !(R s39a s39b s39c s39d s39e s39f s39g s39h) =
        step# s38a s38b s38c s38d s38e s38f s38g s38h
          (k 0x92722c851482353b##) w39
      !(R s40a s40b s40c s40d s40e s40f s40g s40h) =
        step# s39a s39b s39c s39d s39e s39f s39g s39h
          (k 0xa2bfe8a14cf10364##) w40
      !(R s41a s41b s41c s41d s41e s41f s41g s41h) =
        step# s40a s40b s40c s40d s40e s40f s40g s40h
          (k 0xa81a664bbc423001##) w41
      !(R s42a s42b s42c s42d s42e s42f s42g s42h) =
        step# s41a s41b s41c s41d s41e s41f s41g s41h
          (k 0xc24b8b70d0f89791##) w42
      !(R s43a s43b s43c s43d s43e s43f s43g s43h) =
        step# s42a s42b s42c s42d s42e s42f s42g s42h
          (k 0xc76c51a30654be30##) w43
      !(R s44a s44b s44c s44d s44e s44f s44g s44h) =
        step# s43a s43b s43c s43d s43e s43f s43g s43h
          (k 0xd192e819d6ef5218##) w44
      !(R s45a s45b s45c s45d s45e s45f s45g s45h) =
        step# s44a s44b s44c s44d s44e s44f s44g s44h
          (k 0xd69906245565a910##) w45
      !(R s46a s46b s46c s46d s46e s46f s46g s46h) =
        step# s45a s45b s45c s45d s45e s45f s45g s45h
          (k 0xf40e35855771202a##) w46
      !(R s47a s47b s47c s47d s47e s47f s47g s47h) =
        step# s46a s46b s46c s46d s46e s46f s46g s46h
          (k 0x106aa07032bbd1b8##) w47
      !(R s48a s48b s48c s48d s48e s48f s48g s48h) =
        step# s47a s47b s47c s47d s47e s47f s47g s47h
          (k 0x19a4c116b8d2d0c8##) w48
      !(R s49a s49b s49c s49d s49e s49f s49g s49h) =
        step# s48a s48b s48c s48d s48e s48f s48g s48h
          (k 0x1e376c085141ab53##) w49
      !(R s50a s50b s50c s50d s50e s50f s50g s50h) =
        step# s49a s49b s49c s49d s49e s49f s49g s49h
          (k 0x2748774cdf8eeb99##) w50
      !(R s51a s51b s51c s51d s51e s51f s51g s51h) =
        step# s50a s50b s50c s50d s50e s50f s50g s50h
          (k 0x34b0bcb5e19b48a8##) w51
      !(R s52a s52b s52c s52d s52e s52f s52g s52h) =
        step# s51a s51b s51c s51d s51e s51f s51g s51h
          (k 0x391c0cb3c5c95a63##) w52
      !(R s53a s53b s53c s53d s53e s53f s53g s53h) =
        step# s52a s52b s52c s52d s52e s52f s52g s52h
          (k 0x4ed8aa4ae3418acb##) w53
      !(R s54a s54b s54c s54d s54e s54f s54g s54h) =
        step# s53a s53b s53c s53d s53e s53f s53g s53h
          (k 0x5b9cca4f7763e373##) w54
      !(R s55a s55b s55c s55d s55e s55f s55g s55h) =
        step# s54a s54b s54c s54d s54e s54f s54g s54h
          (k 0x682e6ff3d6b2b8a3##) w55
      !(R s56a s56b s56c s56d s56e s56f s56g s56h) =
        step# s55a s55b s55c s55d s55e s55f s55g s55h
          (k 0x748f82ee5defb2fc##) w56
      !(R s57a s57b s57c s57d s57e s57f s57g s57h) =
        step# s56a s56b s56c s56d s56e s56f s56g s56h
          (k 0x78a5636f43172f60##) w57
      !(R s58a s58b s58c s58d s58e s58f s58g s58h) =
        step# s57a s57b s57c s57d s57e s57f s57g s57h
          (k 0x84c87814a1f0ab72##) w58
      !(R s59a s59b s59c s59d s59e s59f s59g s59h) =
        step# s58a s58b s58c s58d s58e s58f s58g s58h
          (k 0x8cc702081a6439ec##) w59
      !(R s60a s60b s60c s60d s60e s60f s60g s60h) =
        step# s59a s59b s59c s59d s59e s59f s59g s59h
          (k 0x90befffa23631e28##) w60
      !(R s61a s61b s61c s61d s61e s61f s61g s61h) =
        step# s60a s60b s60c s60d s60e s60f s60g s60h
          (k 0xa4506cebde82bde9##) w61
      !(R s62a s62b s62c s62d s62e s62f s62g s62h) =
        step# s61a s61b s61c s61d s61e s61f s61g s61h
          (k 0xbef9a3f7b2c67915##) w62
      !(R s63a s63b s63c s63d s63e s63f s63g s63h) =
        step# s62a s62b s62c s62d s62e s62f s62g s62h
          (k 0xc67178f2e372532b##) w63
      !(R s64a s64b s64c s64d s64e s64f s64g s64h) =
        step# s63a s63b s63c s63d s63e s63f s63g s63h
          (k 0xca273eceea26619c##) w64
      !(R s65a s65b s65c s65d s65e s65f s65g s65h) =
        step# s64a s64b s64c s64d s64e s64f s64g s64h
          (k 0xd186b8c721c0c207##) w65
      !(R s66a s66b s66c s66d s66e s66f s66g s66h) =
        step# s65a s65b s65c s65d s65e s65f s65g s65h
          (k 0xeada7dd6cde0eb1e##) w66
      !(R s67a s67b s67c s67d s67e s67f s67g s67h) =
        step# s66a s66b s66c s66d s66e s66f s66g s66h
          (k 0xf57d4f7fee6ed178##) w67
      !(R s68a s68b s68c s68d s68e s68f s68g s68h) =
        step# s67a s67b s67c s67d s67e s67f s67g s67h
          (k 0x06f067aa72176fba##) w68
      !(R s69a s69b s69c s69d s69e s69f s69g s69h) =
        step# s68a s68b s68c s68d s68e s68f s68g s68h
          (k 0x0a637dc5a2c898a6##) w69
      !(R s70a s70b s70c s70d s70e s70f s70g s70h) =
        step# s69a s69b s69c s69d s69e s69f s69g s69h
          (k 0x113f9804bef90dae##) w70
      !(R s71a s71b s71c s71d s71e s71f s71g s71h) =
        step# s70a s70b s70c s70d s70e s70f s70g s70h
          (k 0x1b710b35131c471b##) w71
      !(R s72a s72b s72c s72d s72e s72f s72g s72h) =
        step# s71a s71b s71c s71d s71e s71f s71g s71h
          (k 0x28db77f523047d84##) w72
      !(R s73a s73b s73c s73d s73e s73f s73g s73h) =
        step# s72a s72b s72c s72d s72e s72f s72g s72h
          (k 0x32caab7b40c72493##) w73
      !(R s74a s74b s74c s74d s74e s74f s74g s74h) =
        step# s73a s73b s73c s73d s73e s73f s73g s73h
          (k 0x3c9ebe0a15c9bebc##) w74
      !(R s75a s75b s75c s75d s75e s75f s75g s75h) =
        step# s74a s74b s74c s74d s74e s74f s74g s74h
          (k 0x431d67c49c100d4c##) w75
      !(R s76a s76b s76c s76d s76e s76f s76g s76h) =
        step# s75a s75b s75c s75d s75e s75f s75g s75h
          (k 0x4cc5d4becb3e42b6##) w76
      !(R s77a s77b s77c s77d s77e s77f s77g s77h) =
        step# s76a s76b s76c s76d s76e s76f s76g s76h
          (k 0x597f299cfc657e2a##) w77
      !(R s78a s78b s78c s78d s78e s78f s78g s78h) =
        step# s77a s77b s77c s77d s77e s77f s77g s77h
          (k 0x5fcb6fab3ad6faec##) w78
      !(R s79a s79b s79c s79d s79e s79f s79g s79h) =
        step# s78a s78b s78c s78d s78e s78f s78g s78h
          (k 0x6c44198c4a475817##) w79
  in  R (h0 `p` s79a) (h1 `p` s79b) (h2 `p` s79c) (h3 `p` s79d)
        (h4 `p` s79e) (h5 `p` s79f) (h6 `p` s79g) (h7 `p` s79h)
  where
    p = Exts.plusWord64#
    {-# INLINE p #-}
    k :: Exts.Word# -> Exts.Word64#
    k = Exts.wordToWord64#
    {-# INLINE k #-}

-- rotate right
rotr# :: Exts.Word64# -> Int# -> Exts.Word64#
rotr# x n =
  Exts.uncheckedShiftRL64# x n `Exts.or64#`
  Exts.uncheckedShiftL64# x (64# Exts.-# n)
{-# INLINE rotr# #-}

-- logical right shift
shr# :: Exts.Word64# -> Int# -> Exts.Word64#
shr# = Exts.uncheckedShiftRL64#
{-# INLINE shr# #-}

-- ch(x, y, z) = (x & y) ^ (~x & z)
ch# :: Exts.Word64# -> Exts.Word64# -> Exts.Word64# -> Exts.Word64#
ch# x y z =
  (x `Exts.and64#` y) `Exts.xor64#`
  (Exts.not64# x `Exts.and64#` z)
{-# INLINE ch# #-}

-- maj(x, y, z) = (x & (y | z)) | (y & z)
maj# :: Exts.Word64# -> Exts.Word64# -> Exts.Word64# -> Exts.Word64#
maj# x y z =
  (x `Exts.and64#` (y `Exts.or64#` z)) `Exts.or64#`
  (y `Exts.and64#` z)
{-# INLINE maj# #-}

-- big sigma 0: rotr28 ^ rotr34 ^ rotr39
bsig0# :: Exts.Word64# -> Exts.Word64#
bsig0# x =
  rotr# x 28# `Exts.xor64#` rotr# x 34# `Exts.xor64#` rotr# x 39#
{-# INLINE bsig0# #-}

-- big sigma 1: rotr14 ^ rotr18 ^ rotr41
bsig1# :: Exts.Word64# -> Exts.Word64#
bsig1# x =
  rotr# x 14# `Exts.xor64#` rotr# x 18# `Exts.xor64#` rotr# x 41#
{-# INLINE bsig1# #-}

-- small sigma 0: rotr1 ^ rotr8 ^ shr7
ssig0# :: Exts.Word64# -> Exts.Word64#
ssig0# x =
  rotr# x 1# `Exts.xor64#` rotr# x 8# `Exts.xor64#` shr# x 7#
{-# INLINE ssig0# #-}

-- small sigma 1: rotr19 ^ rotr61 ^ shr6
ssig1# :: Exts.Word64# -> Exts.Word64#
ssig1# x =
  rotr# x 19# `Exts.xor64#` rotr# x 61# `Exts.xor64#` shr# x 6#
{-# INLINE ssig1# #-}

-- round step
step#
  :: Exts.Word64# -> Exts.Word64# -> Exts.Word64# -> Exts.Word64#
  -> Exts.Word64# -> Exts.Word64# -> Exts.Word64# -> Exts.Word64#
  -> Exts.Word64# -> Exts.Word64#
  -> Registers
step# a b c d e f g h k w =
  let !t1 =                h
        `Exts.plusWord64#` bsig1# e
        `Exts.plusWord64#` ch# e f g
        `Exts.plusWord64#` k
        `Exts.plusWord64#` w
      !t2 = bsig0# a `Exts.plusWord64#` maj# a b c
  in  R (t1 `Exts.plusWord64#` t2) a b c (d `Exts.plusWord64#` t1) e f g
{-# INLINE step# #-}

-- initial register state; first 64 bits of the fractional parts of the
-- square roots of the first eight primes
iv :: () -> Registers
iv _ = R
  (Exts.wordToWord64# 0x6a09e667f3bcc908##)
  (Exts.wordToWord64# 0xbb67ae8584caa73b##)
  (Exts.wordToWord64# 0x3c6ef372fe94f82b##)
  (Exts.wordToWord64# 0xa54ff53a5f1d36f1##)
  (Exts.wordToWord64# 0x510e527fade682d1##)
  (Exts.wordToWord64# 0x9b05688c2b3e6c1f##)
  (Exts.wordToWord64# 0x1f83d9abfb41bd6b##)
  (Exts.wordToWord64# 0x5be0cd19137e2179##)

-- serializing ----------------------------------------------------------------

-- | Concat SHA512 state into a ByteString.
cat :: Registers -> BS.ByteString
cat rs = BI.unsafeCreate 64 (cat_into rs)
{-# INLINABLE cat #-}

-- | Serialize SHA512 state to a pointer (big-endian).
cat_into :: Registers -> Ptr Word8 -> IO ()
cat_into (R h0 h1 h2 h3 h4 h5 h6 h7) (Ptr addr) = GHC.IO.IO $ \s0 ->
  case poke64be addr 00# h0 s0 of { s1 ->
  case poke64be addr 08# h1 s1 of { s2 ->
  case poke64be addr 16# h2 s2 of { s3 ->
  case poke64be addr 24# h3 s3 of { s4 ->
  case poke64be addr 32# h4 s4 of { s5 ->
  case poke64be addr 40# h5 s5 of { s6 ->
  case poke64be addr 48# h6 s6 of { s7 ->
  case poke64be addr 56# h7 s7 of { s8 ->
  (# s8, () #)
  }}}}}}}}
{-# INLINE cat_into #-}

poke64be
  :: Exts.Addr#
  -> Int#
  -> Exts.Word64#
  -> Exts.State# Exts.RealWorld
  -> Exts.State# Exts.RealWorld
poke64be a off w s0 =
  case Exts.writeWord8OffAddr# a off (byte# w 56#) s0 of { s1 ->
  case Exts.writeWord8OffAddr# a (off Exts.+# 1#) (byte# w 48#) s1 of { s2 ->
  case Exts.writeWord8OffAddr# a (off Exts.+# 2#) (byte# w 40#) s2 of { s3 ->
  case Exts.writeWord8OffAddr# a (off Exts.+# 3#) (byte# w 32#) s3 of { s4 ->
  case Exts.writeWord8OffAddr# a (off Exts.+# 4#) (byte# w 24#) s4 of { s5 ->
  case Exts.writeWord8OffAddr# a (off Exts.+# 5#) (byte# w 16#) s5 of { s6 ->
  case Exts.writeWord8OffAddr# a (off Exts.+# 6#) (byte# w 8#) s6 of { s7 ->
  Exts.writeWord8OffAddr# a (off Exts.+# 7#) (byte# w 0#) s7
  }}}}}}}
{-# INLINE poke64be #-}

byte# :: Exts.Word64# -> Int# -> Exts.Word8#
byte# w n = Exts.wordToWord8#
  (Exts.word64ToWord# (Exts.uncheckedShiftRL64# w n))
{-# INLINE byte# #-}

-- | Write register state to a pointer (native endian Word64s).
poke_registers :: Ptr Word64 -> Registers -> IO ()
poke_registers (Ptr addr) (R w0 w1 w2 w3 w4 w5 w6 w7) = GHC.IO.IO $ \s0 ->
  case Exts.writeWord64OffAddr# addr 0# w0 s0 of { s1 ->
  case Exts.writeWord64OffAddr# addr 1# w1 s1 of { s2 ->
  case Exts.writeWord64OffAddr# addr 2# w2 s2 of { s3 ->
  case Exts.writeWord64OffAddr# addr 3# w3 s3 of { s4 ->
  case Exts.writeWord64OffAddr# addr 4# w4 s4 of { s5 ->
  case Exts.writeWord64OffAddr# addr 5# w5 s5 of { s6 ->
  case Exts.writeWord64OffAddr# addr 6# w6 s6 of { s7 ->
  case Exts.writeWord64OffAddr# addr 7# w7 s7 of { s8 ->
  (# s8, () #) }}}}}}}}
{-# INLINE poke_registers #-}

-- hmac utilities -------------------------------------------------------------

-- pad registers to block
pad_registers :: Registers -> Block
pad_registers (R w0 w1 w2 w3 w4 w5 w6 w7) = B
  w0 w1 w2 w3 w4 w5 w6 w7
  (Exts.wordToWord64# 0##) (Exts.wordToWord64# 0##) (Exts.wordToWord64# 0##)
  (Exts.wordToWord64# 0##) (Exts.wordToWord64# 0##) (Exts.wordToWord64# 0##)
  (Exts.wordToWord64# 0##) (Exts.wordToWord64# 0##)
{-# INLINE pad_registers #-}

-- pad registers to block, using padding separator and augmented length
-- (assumes existence of a leading block)
-- length = (128 + 64) * 8 = 1536 = 0x600
pad_registers_with_length :: Registers -> Block
pad_registers_with_length (R h0 h1 h2 h3 h4 h5 h6 h7) = B
  h0 h1 h2 h3 h4 h5 h6 h7           -- inner hash
  (Exts.wordToWord64# 0x8000000000000000##) -- padding separator
  (Exts.wordToWord64# 0x0000000000000000##)
  (Exts.wordToWord64# 0x0000000000000000##)
  (Exts.wordToWord64# 0x0000000000000000##)
  (Exts.wordToWord64# 0x0000000000000000##)
  (Exts.wordToWord64# 0x0000000000000000##)
  (Exts.wordToWord64# 0x0000000000000000##) -- high 64 bits of length
  (Exts.wordToWord64# 0x0000000000000600##) -- low 64 bits of length
{-# INLINABLE pad_registers_with_length #-}

xor :: Block -> Exts.Word64# -> Block
xor (B w00 w01 w02 w03 w04 w05 w06 w07 w08 w09 w10 w11 w12 w13 w14 w15) b = B
  (Exts.xor64# w00 b)
  (Exts.xor64# w01 b)
  (Exts.xor64# w02 b)
  (Exts.xor64# w03 b)
  (Exts.xor64# w04 b)
  (Exts.xor64# w05 b)
  (Exts.xor64# w06 b)
  (Exts.xor64# w07 b)
  (Exts.xor64# w08 b)
  (Exts.xor64# w09 b)
  (Exts.xor64# w10 b)
  (Exts.xor64# w11 b)
  (Exts.xor64# w12 b)
  (Exts.xor64# w13 b)
  (Exts.xor64# w14 b)
  (Exts.xor64# w15 b)
{-# INLINE xor #-}

parse_key :: BS.ByteString -> Block
parse_key bs = B
  (w64_zero bs 000) (w64_zero bs 008) (w64_zero bs 016) (w64_zero bs 024)
  (w64_zero bs 032) (w64_zero bs 040) (w64_zero bs 048) (w64_zero bs 056)
  (w64_zero bs 064) (w64_zero bs 072) (w64_zero bs 080) (w64_zero bs 088)
  (w64_zero bs 096) (w64_zero bs 104) (w64_zero bs 112) (w64_zero bs 120)
{-# INLINE parse_key #-}

-- read big-endian Word64#, zero-padding beyond input length
w64_zero :: BS.ByteString -> Int -> Exts.Word64#
w64_zero bs i =
  let !w0 = w8_zero bs i       `Exts.uncheckedShiftL#` 56#
      !w1 = w8_zero bs (i + 1) `Exts.uncheckedShiftL#` 48#
      !w2 = w8_zero bs (i + 2) `Exts.uncheckedShiftL#` 40#
      !w3 = w8_zero bs (i + 3) `Exts.uncheckedShiftL#` 32#
      !w4 = w8_zero bs (i + 4) `Exts.uncheckedShiftL#` 24#
      !w5 = w8_zero bs (i + 5) `Exts.uncheckedShiftL#` 16#
      !w6 = w8_zero bs (i + 6) `Exts.uncheckedShiftL#` 08#
      !w7 = w8_zero bs (i + 7)
  in  Exts.wordToWord64#
        (w0 `Exts.or#` w1 `Exts.or#` w2 `Exts.or#` w3 `Exts.or#`
         w4 `Exts.or#` w5 `Exts.or#` w6 `Exts.or#` w7)
{-# INLINE w64_zero #-}

-- read byte as Word#, returning zero beyond input length
w8_zero :: BS.ByteString -> Int -> Exts.Word#
w8_zero bs@(BI.PS _ _ l) i
  | i < l     = let !(GHC.Word.W8# w) = BU.unsafeIndex bs i
                in  Exts.word8ToWord# w
  | otherwise = 0##
{-# INLINE w8_zero #-}

-- hmac-drbg utilities --------------------------------------------------------

-- | Parse first complete block from v || sep || dat[0:63].
--
--   Requires len(dat) >= 63.
parse_vsb :: Registers -> Word8 -> BS.ByteString -> Block
parse_vsb (R v0 v1 v2 v3 v4 v5 v6 v7) (GHC.Word.W8# sep) dat =
  let !(GHC.Word.W8# b0) = BU.unsafeIndex dat 0
      !(GHC.Word.W8# b1) = BU.unsafeIndex dat 1
      !(GHC.Word.W8# b2) = BU.unsafeIndex dat 2
      !(GHC.Word.W8# b3) = BU.unsafeIndex dat 3
      !(GHC.Word.W8# b4) = BU.unsafeIndex dat 4
      !(GHC.Word.W8# b5) = BU.unsafeIndex dat 5
      !(GHC.Word.W8# b6) = BU.unsafeIndex dat 6
      !w08 =
            Exts.uncheckedShiftL# (Exts.word8ToWord# sep) 56#
            `Exts.or#`
            Exts.uncheckedShiftL# (Exts.word8ToWord# b0) 48#
            `Exts.or#`
            Exts.uncheckedShiftL# (Exts.word8ToWord# b1) 40#
            `Exts.or#`
            Exts.uncheckedShiftL# (Exts.word8ToWord# b2) 32#
            `Exts.or#`
            Exts.uncheckedShiftL# (Exts.word8ToWord# b3) 24#
            `Exts.or#`
            Exts.uncheckedShiftL# (Exts.word8ToWord# b4) 16#
            `Exts.or#`
            Exts.uncheckedShiftL# (Exts.word8ToWord# b5) 8#
            `Exts.or#`
            Exts.word8ToWord# b6
  in  B v0 v1 v2 v3 v4 v5 v6 v7
        (Exts.wordToWord64# w08)
        (word64be dat 07) (word64be dat 15) (word64be dat 23)
        (word64be dat 31) (word64be dat 39) (word64be dat 47) (word64be dat 55)
{-# INLINE parse_vsb #-}

-- | Parse single padding block from v || sep || dat.
--
--   Requires (65 + len(dat)) < 112.
parse_pad1_vsb :: Registers -> Word8 -> BS.ByteString -> Word64 -> Block
parse_pad1_vsb (R v0 v1 v2 v3 v4 v5 v6 v7) sep dat total =
  let !bits = total * 8
      !(GHC.Word.W64# llo) = bits
  in  B v0 v1 v2 v3 v4 v5 v6 v7
        (w64_sdp sep dat 064) (w64_sdp sep dat 072)
        (w64_sdp sep dat 080) (w64_sdp sep dat 088)
        (w64_sdp sep dat 096) (w64_sdp sep dat 104)
        (Exts.wordToWord64# 0##) llo
{-# INLINABLE parse_pad1_vsb #-}

-- | Parse two padding blocks from v || sep || dat.
--
--   Requires 112 <= (65 + len(dat)) < 128.
parse_pad2_vsb
  :: Registers -> Word8 -> BS.ByteString -> Word64 -> (# Block, Block #)
parse_pad2_vsb (R v0 v1 v2 v3 v4 v5 v6 v7) sep dat total =
  let !bits = total * 8
      !z = Exts.wordToWord64# 0##
      !(GHC.Word.W64# llo) = bits
      !b0 = B v0 v1 v2 v3 v4 v5 v6 v7
              (w64_sdp sep dat 064) (w64_sdp sep dat 072)
              (w64_sdp sep dat 080) (w64_sdp sep dat 088)
              (w64_sdp sep dat 096) (w64_sdp sep dat 104)
              (w64_sdp sep dat 112) (w64_sdp sep dat 120)
      !b1 = B z z z z z z z z z z z z z z z llo
  in  (# b0, b1 #)
{-# INLINABLE parse_pad2_vsb #-}

-- Read Word64 at offset i (>= 64) from (sep || dat || 0x80 || zeros).
w64_sdp :: Word8 -> BS.ByteString -> Int -> Exts.Word64#
w64_sdp sep dat i =
  let !(GHC.Word.W8# a) = byte_sdp sep dat i
      !(GHC.Word.W8# b) = byte_sdp sep dat (i + 1)
      !(GHC.Word.W8# c) = byte_sdp sep dat (i + 2)
      !(GHC.Word.W8# d) = byte_sdp sep dat (i + 3)
      !(GHC.Word.W8# e) = byte_sdp sep dat (i + 4)
      !(GHC.Word.W8# f) = byte_sdp sep dat (i + 5)
      !(GHC.Word.W8# g) = byte_sdp sep dat (i + 6)
      !(GHC.Word.W8# h) = byte_sdp sep dat (i + 7)
  in  Exts.wordToWord64#
        (Exts.uncheckedShiftL# (Exts.word8ToWord# a) 56#
         `Exts.or#`
         Exts.uncheckedShiftL# (Exts.word8ToWord# b) 48#
         `Exts.or#`
         Exts.uncheckedShiftL# (Exts.word8ToWord# c) 40#
         `Exts.or#`
         Exts.uncheckedShiftL# (Exts.word8ToWord# d) 32#
         `Exts.or#`
         Exts.uncheckedShiftL# (Exts.word8ToWord# e) 24#
         `Exts.or#`
         Exts.uncheckedShiftL# (Exts.word8ToWord# f) 16#
         `Exts.or#`
         Exts.uncheckedShiftL# (Exts.word8ToWord# g) 8#
         `Exts.or#`
         Exts.word8ToWord# h)
{-# INLINE w64_sdp #-}

-- Read byte at offset i (>= 64) from (sep || dat || 0x80 || zeros).
byte_sdp :: Word8 -> BS.ByteString -> Int -> Word8
byte_sdp sep dat@(BI.PS _ _ l) i
  | i == 64     = sep
  | i < 65 + l  = BU.unsafeIndex dat (i - 65)
  | i == 65 + l = 0x80
  | otherwise   = 0x00
{-# INLINE byte_sdp #-}

