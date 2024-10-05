{-# OPTIONS_GHC -funbox-small-strict-fields #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Crypto.Hash.SHA512
-- Copyright: (c) 2024 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Pure SHA-512 and HMAC-SHA512 implementations for
-- strict and lazy ByteStrings, as specified by RFC's
-- [6234](https://datatracker.ietf.org/doc/html/rfc6234) and
-- [2104](https://datatracker.ietf.org/doc/html/rfc2104).

module Crypto.Hash.SHA512 where

import qualified Data.Bits as B
import Data.Bits ((.|.), (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Builder.Extra as BE
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word64)
import Foreign.ForeignPtr (plusForeignPtr)

-- preliminary utils ----------------------------------------------------------

-- keystroke saver
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- parse strict ByteString in BE order to Word64 (verbatim from
-- Data.Binary)
--
-- invariant:
--   the input bytestring is at least 64 bits in length
unsafe_word64be :: BS.ByteString -> Word64
unsafe_word64be s =
  (fi (s `BU.unsafeIndex` 0) `B.unsafeShiftL` 56) .|.
  (fi (s `BU.unsafeIndex` 1) `B.unsafeShiftL` 48) .|.
  (fi (s `BU.unsafeIndex` 2) `B.unsafeShiftL` 40) .|.
  (fi (s `BU.unsafeIndex` 3) `B.unsafeShiftL` 32) .|.
  (fi (s `BU.unsafeIndex` 4) `B.unsafeShiftL` 24) .|.
  (fi (s `BU.unsafeIndex` 5) `B.unsafeShiftL` 16) .|.
  (fi (s `BU.unsafeIndex` 6) `B.unsafeShiftL`  8) .|.
  (fi (s `BU.unsafeIndex` 7) )
{-# INLINE unsafe_word64be #-}

-- utility types for more efficient ByteString management

data SSPair = SSPair
  {-# UNPACK #-} !BS.ByteString
  {-# UNPACK #-} !BS.ByteString

data SLPair = SLPair {-# UNPACK #-} !BS.ByteString !BL.ByteString

data WSPair = WSPair {-# UNPACK #-} !Word64 {-# UNPACK #-} !BS.ByteString

-- unsafe version of splitAt that does no bounds checking
--
-- invariant:
--   0 <= n <= l
unsafe_splitAt :: Int -> BS.ByteString -> SSPair
unsafe_splitAt n (BI.BS x l) =
  SSPair (BI.BS x n) (BI.BS (plusForeignPtr x n) (l - n))

-- variant of Data.ByteString.Lazy.splitAt that returns the initial
-- component as a strict, unboxed ByteString
splitAt128 :: BL.ByteString -> SLPair
splitAt128 = splitAt' (128 :: Int) where
  splitAt' _ BLI.Empty        = SLPair mempty BLI.Empty
  splitAt' n (BLI.Chunk c cs) =
    if    n < BS.length c
    then
      -- n < BS.length c, so unsafe_splitAt is safe
      let !(SSPair c0 c1) = unsafe_splitAt n c
      in  SLPair c0 (BLI.Chunk c1 cs)
    else
      let SLPair cs' cs'' = splitAt' (n - BS.length c) cs
      in  SLPair (c <> cs') cs''

-- variant of Data.ByteString.splitAt that behaves like an incremental
-- Word64 parser
--
-- invariant:
--   the input bytestring is at least 64 bits in length
unsafe_parseWsPair :: BS.ByteString -> WSPair
unsafe_parseWsPair (BI.BS x l) =
  WSPair (unsafe_word64be (BI.BS x 8)) (BI.BS (plusForeignPtr x 8) (l - 8))
{-# INLINE unsafe_parseWsPair #-}

-- message padding and parsing ------------------------------------------------
-- https://datatracker.ietf.org/doc/html/rfc6234#section-4.1

-- k such that (l + 1 + k) mod 128 = 112
sol :: Word64 -> Word64
sol l =
  let r = 112 - fi l `mod` 128 - 1 :: Integer -- fi prevents underflow
  in  fi (if r < 0 then r + 128 else r)

-- XX doesn't properly handle (> maxBound :: Word64) length

-- RFC 6234 4.1 (strict)
pad :: BS.ByteString -> BS.ByteString
pad m = BL.toStrict . BSB.toLazyByteString $ padded where
  l = fi (BS.length m)
  padded = BSB.byteString m <> fill (sol l) (BSB.word8 0x80)

  fill j !acc
    | j == 0 = acc <> BSB.word64BE 0x00 <> BSB.word64BE (l * 8)
    | otherwise = fill (pred j) (acc <> BSB.word8 0x00)

-- RFC 6234 4.1 (lazy)
pad_lazy :: BL.ByteString -> BL.ByteString
pad_lazy (BL.toChunks -> m) = BL.fromChunks (walk 0 m) where
  walk !l bs = case bs of
    (c:cs) -> c : walk (l + fi (BS.length c)) cs
    [] -> padding l (sol l) (BSB.word8 0x80)

  padding l k bs
    | k == 0 =
          pure
        . BL.toStrict
          -- more efficient for small builder
        . BE.toLazyByteStringWith
            (BE.safeStrategy 128 BE.smallChunkSize) mempty
        $ bs <> BSB.word64BE 0x00 <> BSB.word64BE (l * 8)
    | otherwise =
        let nacc = bs <> BSB.word8 0x00
        in  padding l (pred k) nacc

-- functions and constants used -----------------------------------------------
-- https://datatracker.ietf.org/doc/html/rfc6234#section-5.1

ch :: Word64 -> Word64 -> Word64 -> Word64
ch x y z = (x .&. y) `B.xor` (B.complement x .&. z)
{-# INLINE ch #-}

-- credit to SHA authors for the following optimisation. their text:
--
-- > note:
-- >   the original functions is (x & y) ^ (x & z) ^ (y & z)
-- >   if you fire off truth tables, this is equivalent to
-- >     (x & y) | (x & z) | (y & z)
-- >   which you can the use distribution on:
-- >     (x & (y | z)) | (y & z)
-- >   which saves us one operation.
maj :: Word64 -> Word64 -> Word64 -> Word64
maj x y z = (x .&. (y .|. z)) .|. (y .&. z)
{-# INLINE maj #-}

bsig0 :: Word64 -> Word64
bsig0 x = B.rotateR x 2 `B.xor` B.rotateR x 13 `B.xor` B.rotateR x 22
{-# INLINE bsig0 #-}

bsig1 :: Word64 -> Word64
bsig1 x = B.rotateR x 6 `B.xor` B.rotateR x 11 `B.xor` B.rotateR x 25
{-# INLINE bsig1 #-}

ssig0 :: Word64 -> Word64
ssig0 x = B.rotateR x 7 `B.xor` B.rotateR x 18 `B.xor` B.unsafeShiftR x 3
{-# INLINE ssig0 #-}

ssig1 :: Word64 -> Word64
ssig1 x = B.rotateR x 17 `B.xor` B.rotateR x 19 `B.xor` B.unsafeShiftR x 10
{-# INLINE ssig1 #-}

data Schedule = Schedule {
    w00 :: !Word64, w01 :: !Word64, w02 :: !Word64, w03 :: !Word64
  , w04 :: !Word64, w05 :: !Word64, w06 :: !Word64, w07 :: !Word64
  , w08 :: !Word64, w09 :: !Word64, w10 :: !Word64, w11 :: !Word64
  , w12 :: !Word64, w13 :: !Word64, w14 :: !Word64, w15 :: !Word64
  , w16 :: !Word64, w17 :: !Word64, w18 :: !Word64, w19 :: !Word64
  , w20 :: !Word64, w21 :: !Word64, w22 :: !Word64, w23 :: !Word64
  , w24 :: !Word64, w25 :: !Word64, w26 :: !Word64, w27 :: !Word64
  , w28 :: !Word64, w29 :: !Word64, w30 :: !Word64, w31 :: !Word64
  , w32 :: !Word64, w33 :: !Word64, w34 :: !Word64, w35 :: !Word64
  , w36 :: !Word64, w37 :: !Word64, w38 :: !Word64, w39 :: !Word64
  , w40 :: !Word64, w41 :: !Word64, w42 :: !Word64, w43 :: !Word64
  , w44 :: !Word64, w45 :: !Word64, w46 :: !Word64, w47 :: !Word64
  , w48 :: !Word64, w49 :: !Word64, w50 :: !Word64, w51 :: !Word64
  , w52 :: !Word64, w53 :: !Word64, w54 :: !Word64, w55 :: !Word64
  , w56 :: !Word64, w57 :: !Word64, w58 :: !Word64, w59 :: !Word64
  , w60 :: !Word64, w61 :: !Word64, w62 :: !Word64, w63 :: !Word64
  , w64 :: !Word64, w65 :: !Word64, w66 :: !Word64, w67 :: !Word64
  , w68 :: !Word64, w69 :: !Word64, w70 :: !Word64, w71 :: !Word64
  , w72 :: !Word64, w73 :: !Word64, w74 :: !Word64, w75 :: !Word64
  , w76 :: !Word64, w77 :: !Word64, w78 :: !Word64, w79 :: !Word64
  }

-- initialization -------------------------------------------------------------
-- https://datatracker.ietf.org/doc/html/rfc6234#section-6.1

data Registers = Registers {
    h0 :: !Word64, h1 :: !Word64, h2 :: !Word64, h3 :: !Word64
  , h4 :: !Word64, h5 :: !Word64, h6 :: !Word64, h7 :: !Word64
  }

-- first 64 bits of the fractional parts of the square roots of the
-- first eight primes
iv :: Registers
iv = Registers
  0x6a09e667f3bcc908 0xbb67ae8584caa73b 0x3c6ef372fe94f82b 0xa54ff53a5f1d36f1
  0x510e527fade682d1 0x9b05688c2b3e6c1f 0x1f83d9abfb41bd6b 0x5be0cd19137e2179

-- processing -----------------------------------------------------------------
-- https://datatracker.ietf.org/doc/html/rfc6234#section-6.2

data Block = Block {
    m00 :: !Word64, m01 :: !Word64, m02 :: !Word64, m03 :: !Word64
  , m04 :: !Word64, m05 :: !Word64, m06 :: !Word64, m07 :: !Word64
  , m08 :: !Word64, m09 :: !Word64, m10 :: !Word64, m11 :: !Word64
  , m12 :: !Word64, m13 :: !Word64, m14 :: !Word64, m15 :: !Word64
  }

-- parse strict bytestring to block
--
-- invariant:
--   the input bytestring is exactly 1024 bits long
unsafe_parse :: BS.ByteString -> Block
unsafe_parse bs =
  let !(WSPair m00 t00) = unsafe_parseWsPair bs
      !(WSPair m01 t01) = unsafe_parseWsPair t00
      !(WSPair m02 t02) = unsafe_parseWsPair t01
      !(WSPair m03 t03) = unsafe_parseWsPair t02
      !(WSPair m04 t04) = unsafe_parseWsPair t03
      !(WSPair m05 t05) = unsafe_parseWsPair t04
      !(WSPair m06 t06) = unsafe_parseWsPair t05
      !(WSPair m07 t07) = unsafe_parseWsPair t06
      !(WSPair m08 t08) = unsafe_parseWsPair t07
      !(WSPair m09 t09) = unsafe_parseWsPair t08
      !(WSPair m10 t10) = unsafe_parseWsPair t09
      !(WSPair m11 t11) = unsafe_parseWsPair t10
      !(WSPair m12 t12) = unsafe_parseWsPair t11
      !(WSPair m13 t13) = unsafe_parseWsPair t12
      !(WSPair m14 t14) = unsafe_parseWsPair t13
      !(WSPair m15 t15) = unsafe_parseWsPair t14
  in  if   BS.null t15
      then Block {..}
      else error "ppad-sha512: internal error (bytes remaining)"

-- RFC 6234 6.2 step 1
prepare_schedule :: Block -> Schedule
prepare_schedule Block {..} = Schedule {..} where
  w00 = m00; w01 = m01; w02 = m02; w03 = m03
  w04 = m04; w05 = m05; w06 = m06; w07 = m07
  w08 = m08; w09 = m09; w10 = m10; w11 = m11
  w12 = m12; w13 = m13; w14 = m14; w15 = m15
  w16 = ssig1 w14 + w09 + ssig0 w01 + w00
  w17 = ssig1 w15 + w10 + ssig0 w02 + w01
  w18 = ssig1 w16 + w11 + ssig0 w03 + w02
  w19 = ssig1 w17 + w12 + ssig0 w04 + w03
  w20 = ssig1 w18 + w13 + ssig0 w05 + w04
  w21 = ssig1 w19 + w14 + ssig0 w06 + w05
  w22 = ssig1 w20 + w15 + ssig0 w07 + w06
  w23 = ssig1 w21 + w16 + ssig0 w08 + w07
  w24 = ssig1 w22 + w17 + ssig0 w09 + w08
  w25 = ssig1 w23 + w18 + ssig0 w10 + w09
  w26 = ssig1 w24 + w19 + ssig0 w11 + w10
  w27 = ssig1 w25 + w20 + ssig0 w12 + w11
  w28 = ssig1 w26 + w21 + ssig0 w13 + w12
  w29 = ssig1 w27 + w22 + ssig0 w14 + w13
  w30 = ssig1 w28 + w23 + ssig0 w15 + w14
  w31 = ssig1 w29 + w24 + ssig0 w16 + w15
  w32 = ssig1 w30 + w25 + ssig0 w17 + w16
  w33 = ssig1 w31 + w26 + ssig0 w18 + w17
  w34 = ssig1 w32 + w27 + ssig0 w19 + w18
  w35 = ssig1 w33 + w28 + ssig0 w20 + w19
  w36 = ssig1 w34 + w29 + ssig0 w21 + w20
  w37 = ssig1 w35 + w30 + ssig0 w22 + w21
  w38 = ssig1 w36 + w31 + ssig0 w23 + w22
  w39 = ssig1 w37 + w32 + ssig0 w24 + w23
  w40 = ssig1 w38 + w33 + ssig0 w25 + w24
  w41 = ssig1 w39 + w34 + ssig0 w26 + w25
  w42 = ssig1 w40 + w35 + ssig0 w27 + w26
  w43 = ssig1 w41 + w36 + ssig0 w28 + w27
  w44 = ssig1 w42 + w37 + ssig0 w29 + w28
  w45 = ssig1 w43 + w38 + ssig0 w30 + w29
  w46 = ssig1 w44 + w39 + ssig0 w31 + w30
  w47 = ssig1 w45 + w40 + ssig0 w32 + w31
  w48 = ssig1 w46 + w41 + ssig0 w33 + w32
  w49 = ssig1 w47 + w42 + ssig0 w34 + w33
  w50 = ssig1 w48 + w43 + ssig0 w35 + w34
  w51 = ssig1 w49 + w44 + ssig0 w36 + w35
  w52 = ssig1 w50 + w45 + ssig0 w37 + w36
  w53 = ssig1 w51 + w46 + ssig0 w38 + w37
  w54 = ssig1 w52 + w47 + ssig0 w39 + w38
  w55 = ssig1 w53 + w48 + ssig0 w40 + w39
  w56 = ssig1 w54 + w49 + ssig0 w41 + w40
  w57 = ssig1 w55 + w50 + ssig0 w42 + w41
  w58 = ssig1 w56 + w51 + ssig0 w43 + w42
  w59 = ssig1 w57 + w52 + ssig0 w44 + w43
  w60 = ssig1 w58 + w53 + ssig0 w45 + w44
  w61 = ssig1 w59 + w54 + ssig0 w46 + w45
  w62 = ssig1 w60 + w55 + ssig0 w47 + w46
  w63 = ssig1 w61 + w56 + ssig0 w48 + w47
  w64 = ssig1 w62 + w57 + ssig0 w49 + w48
  w65 = ssig1 w63 + w58 + ssig0 w50 + w49
  w66 = ssig1 w64 + w59 + ssig0 w51 + w50
  w67 = ssig1 w65 + w60 + ssig0 w52 + w51
  w68 = ssig1 w66 + w61 + ssig0 w53 + w52
  w69 = ssig1 w67 + w62 + ssig0 w54 + w53
  w70 = ssig1 w68 + w63 + ssig0 w55 + w54
  w71 = ssig1 w69 + w64 + ssig0 w56 + w55
  w72 = ssig1 w70 + w65 + ssig0 w57 + w56
  w73 = ssig1 w71 + w66 + ssig0 w58 + w57
  w74 = ssig1 w72 + w67 + ssig0 w59 + w58
  w75 = ssig1 w73 + w68 + ssig0 w60 + w59
  w76 = ssig1 w74 + w69 + ssig0 w61 + w60
  w77 = ssig1 w75 + w70 + ssig0 w62 + w61
  w78 = ssig1 w76 + w71 + ssig0 w63 + w62
  w79 = ssig1 w77 + w72 + ssig0 w64 + w63

-- RFC 6234 6.2 steps 2, 3, 4
block_hash :: Registers -> Schedule -> Registers
block_hash r00@Registers {..} Schedule {..} =
  -- constants are the first 64 bits of the fractional parts of the
  -- cube roots of the first eighty prime numbers
  let r01 = step r00 0x428a2f98d728ae22 w00
      r02 = step r01 0x7137449123ef65cd w01
      r03 = step r02 0xb5c0fbcfec4d3b2f w02
      r04 = step r03 0xe9b5dba58189dbbc w03
      r05 = step r04 0x3956c25bf348b538 w04
      r06 = step r05 0x59f111f1b605d019 w05
      r07 = step r06 0x923f82a4af194f9b w06
      r08 = step r07 0xab1c5ed5da6d8118 w07
      r09 = step r08 0xd807aa98a3030242 w08
      r10 = step r09 0x12835b0145706fbe w09
      r11 = step r10 0x243185be4ee4b28c w10
      r12 = step r11 0x550c7dc3d5ffb4e2 w11
      r13 = step r12 0x72be5d74f27b896f w12
      r14 = step r13 0x80deb1fe3b1696b1 w13
      r15 = step r14 0x9bdc06a725c71235 w14
      r16 = step r15 0xc19bf174cf692694 w15
      r17 = step r16 0xe49b69c19ef14ad2 w16
      r18 = step r17 0xefbe4786384f25e3 w17
      r19 = step r18 0x0fc19dc68b8cd5b5 w18
      r20 = step r19 0x240ca1cc77ac9c65 w19
      r21 = step r20 0x2de92c6f592b0275 w20
      r22 = step r21 0x4a7484aa6ea6e483 w21
      r23 = step r22 0x5cb0a9dcbd41fbd4 w22
      r24 = step r23 0x76f988da831153b5 w23
      r25 = step r24 0x983e5152ee66dfab w24
      r26 = step r25 0xa831c66d2db43210 w25
      r27 = step r26 0xb00327c898fb213f w26
      r28 = step r27 0xbf597fc7beef0ee4 w27
      r29 = step r28 0xc6e00bf33da88fc2 w28
      r30 = step r29 0xd5a79147930aa725 w29
      r31 = step r30 0x06ca6351e003826f w30
      r32 = step r31 0x142929670a0e6e70 w31
      r33 = step r32 0x27b70a8546d22ffc w32
      r34 = step r33 0x2e1b21385c26c926 w33
      r35 = step r34 0x4d2c6dfc5ac42aed w34
      r36 = step r35 0x53380d139d95b3df w35
      r37 = step r36 0x650a73548baf63de w36
      r38 = step r37 0x766a0abb3c77b2a8 w37
      r39 = step r38 0x81c2c92e47edaee6 w38
      r40 = step r39 0x92722c851482353b w39
      r41 = step r40 0xa2bfe8a14cf10364 w40
      r42 = step r41 0xa81a664bbc423001 w41
      r43 = step r42 0xc24b8b70d0f89791 w42
      r44 = step r43 0xc76c51a30654be30 w43
      r45 = step r44 0xd192e819d6ef5218 w44
      r46 = step r45 0xd69906245565a910 w45
      r47 = step r46 0xf40e35855771202a w46
      r48 = step r47 0x106aa07032bbd1b8 w47
      r49 = step r48 0x19a4c116b8d2d0c8 w48
      r50 = step r49 0x1e376c085141ab53 w49
      r51 = step r50 0x2748774cdf8eeb99 w50
      r52 = step r51 0x34b0bcb5e19b48a8 w51
      r53 = step r52 0x391c0cb3c5c95a63 w52
      r54 = step r53 0x4ed8aa4ae3418acb w53
      r55 = step r54 0x5b9cca4f7763e373 w54
      r56 = step r55 0x682e6ff3d6b2b8a3 w55
      r57 = step r56 0x748f82ee5defb2fc w56
      r58 = step r57 0x78a5636f43172f60 w57
      r59 = step r58 0x84c87814a1f0ab72 w58
      r60 = step r59 0x8cc702081a6439ec w59
      r61 = step r60 0x90befffa23631e28 w60
      r62 = step r61 0xa4506cebde82bde9 w61
      r63 = step r62 0xbef9a3f7b2c67915 w62
      r64 = step r63 0xc67178f2e372532b w63
      r65 = step r64 0xca273eceea26619c w64
      r66 = step r65 0xd186b8c721c0c207 w65
      r67 = step r66 0xeada7dd6cde0eb1e w66
      r68 = step r67 0xf57d4f7fee6ed178 w67
      r69 = step r68 0x06f067aa72176fba w68
      r70 = step r69 0x0a637dc5a2c898a6 w69
      r71 = step r70 0x113f9804bef90dae w70
      r72 = step r71 0x1b710b35131c471b w71
      r73 = step r72 0x28db77f523047d84 w72
      r74 = step r73 0x32caab7b40c72493 w73
      r75 = step r74 0x3c9ebe0a15c9bebc w74
      r76 = step r75 0x431d67c49c100d4c w75
      r77 = step r76 0x4cc5d4becb3e42b6 w76
      r78 = step r77 0x597f299cfc657e2a w77
      r79 = step r78 0x5fcb6fab3ad6faec w78
      r80 = step r79 0x6c44198c4a475817 w79
      !(Registers a b c d e f g h) = r80
  in  Registers
        (a + h0) (b + h1) (c + h2) (d + h3)
        (e + h4) (f + h5) (g + h6) (h + h7)

step :: Registers -> Word64 -> Word64 -> Registers
step (Registers a b c d e f g h) k w =
  let t1 = h + bsig1 e + ch e f g + k + w
      t2 = bsig0 a + maj a b c
  in  Registers (t1 + t2) a b c (d + t1) e f g

-- RFC 6234 6.2 block pipeline
--
-- invariant:
--   the input bytestring is exactly 1024 bits in length
unsafe_hash_alg :: Registers -> BS.ByteString -> Registers
unsafe_hash_alg rs bs = block_hash rs (prepare_schedule (unsafe_parse bs))

-- register concatenation
cat :: Registers -> BS.ByteString
cat Registers {..} =
    BL.toStrict
    -- more efficient for small builder
  . BE.toLazyByteStringWith (BE.safeStrategy 128 BE.smallChunkSize) mempty
  $ mconcat [
        BSB.word64BE h0, BSB.word64BE h1, BSB.word64BE h2, BSB.word64BE h3
      , BSB.word64BE h4, BSB.word64BE h5, BSB.word64BE h6, BSB.word64BE h7
      ]

-- | Compute a condensed representation of a strict bytestring via
--   SHA-512.
--
--   The 512-bit output digest is returned as a strict bytestring.
--
--   >>> hash "strict bytestring input"
--   "<strict 512-bit message digest>"
hash :: BS.ByteString -> BS.ByteString
hash bs = cat (go iv (pad bs)) where
  -- proof that 'go' always terminates safely:
  --
  -- let b = pad bs
  -- then length(b) = n * 1024 bits for some n >= 0                 (1)
  go :: Registers -> BS.ByteString -> Registers
  go !acc b
    -- if n == 0, then 'go' terminates safely                       (2)
    | BS.null b = acc
    -- if n > 0, then
    --
    -- let (c, r) = unsafe_splitAt 128 b
    -- then length(c) == 1024 bits                                  by (1)
    --      length(r) == m * 1024 bits for some m >= 0              by (1)
    --
    -- note 'unsafe_hash_alg' terminates safely for bytestring      (3)
    -- input of exactly 1024 bits in length
    --
    -- length(c) == 1024
    --   => 'unsafe_hash_alg' terminates safely                     by (3)
    --   => 'go' terminates safely                                  (4)
    -- length(r) == m * 1024 bits for m >= 0
    --   => next invocation of 'go' terminates safely               by (2), (4)
    --
    -- then by induction, 'go' always terminates safely (QED)
    | otherwise = case unsafe_splitAt 128 b of
        SSPair c r -> go (unsafe_hash_alg acc c) r

-- | Compute a condensed representation of a lazy bytestring via
--   SHA-512.
--
--   The 512-bit output digest is returned as a strict bytestring.
--
--   >>> hash_lazy "lazy bytestring input"
--   "<strict 512-bit message digest>"
hash_lazy :: BL.ByteString -> BS.ByteString
hash_lazy bl = cat (go iv (pad_lazy bl)) where
  -- proof of safety proceeds analogously
  go :: Registers -> BL.ByteString -> Registers
  go !acc bs
    | BL.null bs = acc
    | otherwise = case splitAt128 bs of
        SLPair c r -> go (unsafe_hash_alg acc c) r

