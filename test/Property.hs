{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MagicHash #-}

module Property (
    properties
  ) where

import Data.Bits ((.|.))
import qualified Data.Bits as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8, Word64)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr)
import qualified GHC.Exts as Exts
import qualified GHC.Word
import Crypto.Hash.SHA512
import Crypto.Hash.SHA512.Internal (Registers(..))
import qualified Crypto.Hash.SHA512.Pure as Pure
import Test.Tasty
import qualified Test.Tasty.HUnit as H
import qualified Test.Tasty.QuickCheck as Q
import Test.QuickCheck.Instances.ByteString ()

-- strict/lazy hash equivalence
hash_equiv :: BS.ByteString -> Bool
hash_equiv bs = hash bs == hash_lazy (BL.fromStrict bs)

-- strict/lazy hmac equivalence
hmac_equiv :: BS.ByteString -> BS.ByteString -> Bool
hmac_equiv k m = hmac k m == hmac_lazy k (BL.fromStrict m)

-- hash output is always 64 bytes
hash_length :: BS.ByteString -> Bool
hash_length bs = BS.length (hash bs) == 64

-- hmac output is always 64 bytes
hmac_length :: BS.ByteString -> BS.ByteString -> Bool
hmac_length k m = let MAC h = hmac k m in BS.length h == 64

-- hash_lazy produces same result regardless of chunking
hash_chunking :: BS.ByteString -> [Q.Positive Int] -> Bool
hash_chunking bs chunks =
  let lazy_single = BL.fromStrict bs
      lazy_chunked = BL.fromChunks (chunk_by (map Q.getPositive chunks) bs)
  in  hash_lazy lazy_single == hash_lazy lazy_chunked

-- hmac_lazy produces same result regardless of chunking
hmac_chunking :: BS.ByteString -> BS.ByteString -> [Q.Positive Int] -> Bool
hmac_chunking k m chunks =
  let lazy_single = BL.fromStrict m
      lazy_chunked = BL.fromChunks (chunk_by (map Q.getPositive chunks) m)
  in  hmac_lazy k lazy_single == hmac_lazy k lazy_chunked

-- dispatching vs. pure implementations ------------------------------------
--
-- on hosts with the ARM SHA512 extension, the public functions use it, so
-- these compare it against the pure Haskell implementation.

-- messages spanning several blocks
newtype Msg = Msg BS.ByteString
  deriving Show

instance Q.Arbitrary Msg where
  arbitrary = do
    l <- Q.chooseInt (0, 8 * 128)
    Msg . BS.pack <$> Q.vectorOf l Q.arbitrary

-- register-sized strings
newtype Bytes64 = Bytes64 BS.ByteString
  deriving Show

instance Q.Arbitrary Bytes64 where
  arbitrary = Bytes64 . BS.pack <$> Q.vectorOf 64 Q.arbitrary

-- big-endian words of a 64-byte string, as registers
to_registers :: BS.ByteString -> Registers
to_registers bs = R (w 0) (w 1) (w 2) (w 3) (w 4) (w 5) (w 6) (w 7) where
  w :: Int -> Exts.Word64#
  w i = case word64 (BS.take 8 (BS.drop (8 * i) bs)) of
    GHC.Word.W64# x -> x
  word64 :: BS.ByteString -> Word64
  word64 = BS.foldl' (\acc b -> acc `B.shiftL` 8 .|. fromIntegral b) 0

-- run a destination-pointer HMAC, returning the big-endian digest
run_ptr :: (Ptr Word64 -> Ptr Word64 -> IO ()) -> IO BS.ByteString
run_ptr act = allocaBytes 64 $ \rp -> allocaBytes 128 $ \bp -> do
  act rp bp
  ws <- peekArray 8 rp
  pure (BL.toStrict (BSB.toLazyByteString (foldMap BSB.word64BE ws)))

unmac :: MAC -> BS.ByteString
unmac (MAC m) = m

-- deterministic input of the given length
msg :: Int -> BS.ByteString
msg l = BS.pack (fmap fromIntegral (take l [(7 :: Int), 20 ..]))

hash_matches_pure :: Msg -> Bool
hash_matches_pure (Msg m) = hash m == Pure.hash m

hmac_matches_pure :: Msg -> Msg -> Bool
hmac_matches_pure (Msg k) (Msg m) = unmac (hmac k m) == Pure.hmac k m

hmac_rr_matches_hmac :: Bytes64 -> Bytes64 -> Q.Property
hmac_rr_matches_hmac (Bytes64 k) (Bytes64 m) = Q.ioProperty $ do
  let expected = unmac (hmac k m)
  a <- run_ptr (\rp bp -> _hmac_rr rp bp (to_registers k) (to_registers m))
  b <- run_ptr (\rp bp ->
         Pure._hmac_rr rp bp (to_registers k) (to_registers m))
  pure (a == expected && b == expected)

hmac_rsb_matches_hmac :: Bytes64 -> Bytes64 -> Word8 -> Msg -> Q.Property
hmac_rsb_matches_hmac (Bytes64 k) (Bytes64 v) sep (Msg dat) =
  Q.ioProperty (rsb_matches k v sep dat)

rsb_matches
  :: BS.ByteString -> BS.ByteString -> Word8 -> BS.ByteString -> IO Bool
rsb_matches k v sep dat = do
  let expected = unmac (hmac k (v <> BS.singleton sep <> dat))
  a <- run_ptr (\rp bp ->
         _hmac_rsb rp bp (to_registers k) (to_registers v) sep dat)
  b <- run_ptr (\rp bp ->
         Pure._hmac_rsb rp bp (to_registers k) (to_registers v) sep dat)
  pure (a == expected && b == expected)

-- every input length through three blocks, hitting each padding case
hash_all_lengths :: TestTree
hash_all_lengths = H.testCase "hash ~ Pure.hash (all lengths)" $
  H.assertBool mempty $
    all (\l -> hash (msg l) == Pure.hash (msg l)) [0 .. 3 * 128]

hmac_all_lengths :: TestTree
hmac_all_lengths = H.testCase "hmac ~ Pure.hmac (all lengths)" $
  H.assertBool mempty $
    all (\l -> unmac (hmac (msg 64) (msg l)) == Pure.hmac (msg 64) (msg l))
      [0 .. 3 * 128]

hmac_all_key_lengths :: TestTree
hmac_all_key_lengths = H.testCase "hmac ~ Pure.hmac (all key lengths)" $
  H.assertBool mempty $
    all (\l -> unmac (hmac (msg l) (msg 3)) == Pure.hmac (msg l) (msg 3))
      [0 .. 2 * 128 + 1]

hmac_rsb_all_lengths :: TestTree
hmac_rsb_all_lengths = H.testCase "_hmac_rsb ~ hmac (all lengths)" $ do
  oks <- traverse (rsb_matches (msg 64) (BS.reverse (msg 64)) 0x01 . msg)
           [0 .. 3 * 128]
  H.assertBool mempty (and oks)

chunk_by :: [Int] -> BS.ByteString -> [BS.ByteString]
chunk_by _ bs | BS.null bs = []
chunk_by [] bs = [bs]
chunk_by (n:ns) bs =
  let (h, t) = BS.splitAt n bs
  in  h : chunk_by ns t

properties :: TestTree
properties = testGroup "properties" [
    Q.testProperty "hash == hash_lazy" $
      Q.withMaxSuccess 1000 hash_equiv
  , Q.testProperty "hmac == hmac_lazy" $
      Q.withMaxSuccess 1000 hmac_equiv
  , Q.testProperty "hash output is 64 bytes" $
      Q.withMaxSuccess 1000 hash_length
  , Q.testProperty "hmac output is 64 bytes" $
      Q.withMaxSuccess 1000 hmac_length
  , Q.testProperty "hash_lazy chunking-invariant" $
      Q.withMaxSuccess 1000 hash_chunking
  , Q.testProperty "hmac_lazy chunking-invariant" $
      Q.withMaxSuccess 1000 hmac_chunking
  , testGroup "dispatch ~ pure" [
      Q.testProperty "hash ~ Pure.hash" $
        Q.withMaxSuccess 1000 hash_matches_pure
    , Q.testProperty "hmac ~ Pure.hmac" $
        Q.withMaxSuccess 1000 hmac_matches_pure
    , Q.testProperty "_hmac_rr k m ~ hmac (cat k) (cat m)" $
        Q.withMaxSuccess 1000 hmac_rr_matches_hmac
    , Q.testProperty "_hmac_rsb k v s d ~ hmac (cat k) (cat v <> s <> d)" $
        Q.withMaxSuccess 1000 hmac_rsb_matches_hmac
    , hash_all_lengths
    , hmac_all_lengths
    , hmac_all_key_lengths
    , hmac_rsb_all_lengths
    ]
  ]
