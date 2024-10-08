{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Crypto.Hash.SHA512 as SHA512
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Test.Tasty
import Test.Tasty.HUnit
import qualified Wycheproof as W

main :: IO ()
main = do
  wycheproof <- TIO.readFile "etc/wycheproof_hmac_sha512.json"
  case A.decodeStrictText wycheproof :: Maybe W.Wycheproof of
    Nothing -> error "couldn't parse wycheproof vectors"
    Just w  -> defaultMain $ testGroup "ppad-sha512" [
        unit_tests
      , wycheproof_tests w
      ]

wycheproof_tests :: W.Wycheproof -> TestTree
wycheproof_tests W.Wycheproof {..} = testGroup "wycheproof vectors (hmac)" $
  fmap execute_group wp_testGroups

execute_group :: W.MacTestGroup -> TestTree
execute_group W.MacTestGroup {..} =
    testGroup msg (fmap (execute mtg_tagSize) mtg_tests)
  where
    msg = "keysize " <> show mtg_keySize <> ", tagsize " <> show mtg_tagSize

execute :: Int -> W.MacTest -> TestTree
execute tag_size W.MacTest {..} = testCase t_msg $ do
    let key = B16.decodeLenient (TE.encodeUtf8 mt_key)
        msg = B16.decodeLenient (TE.encodeUtf8 mt_msg)
        pec = B16.decodeLenient (TE.encodeUtf8 mt_tag)
        out = BS.take bytes (SHA512.hmac key msg)
    if   mt_result == "invalid"
    then assertBool "invalid" (pec /= out)
    else assertEqual mempty pec out
  where
    t_msg = "test " <> show mt_tcId -- XX embellish
    bytes = tag_size `div` 8

unit_tests :: TestTree
unit_tests = testGroup "unit tests" [
    testGroup "hash" [
      cmp_hash "hv0" hv0_put hv0_pec
    , cmp_hash "hv1" hv1_put hv1_pec
    , cmp_hash "hv2" hv2_put hv2_pec
    , cmp_hash "hv3" hv3_put hv3_pec
    , cmp_hash "hv4" hv4_put hv4_pec
    ]
  , testGroup "hash_lazy" [
      cmp_hash_lazy "hv0" hv0_put hv0_pec
    , cmp_hash_lazy "hv1" hv1_put hv1_pec
    , cmp_hash_lazy "hv2" hv2_put hv2_pec
    , cmp_hash_lazy "hv3" hv3_put hv3_pec
    , cmp_hash_lazy "hv4" hv4_put hv4_pec
    ]
  -- uncomment me to run (slow)

  -- , testGroup "hash_lazy (1GB input)" [
  --     testCase "hv5" $ do
  --       let out = B16.encode (SHA512.hash_lazy hv5_put)
  --       assertEqual mempty hv5_pec out
  --   ]
  , testGroup "hmac" [
      cmp_hmac "hmv1" hmv1_key hmv1_put hmv1_pec
    , cmp_hmac "hmv2" hmv2_key hmv2_put hmv2_pec
    , cmp_hmac "hmv3" hmv3_key hmv3_put hmv3_pec
    , cmp_hmac "hmv4" hmv4_key hmv4_put hmv4_pec
    , testCase "hmv5" $ do
        let out = BS.take 32 $ B16.encode (SHA512.hmac hmv5_key hmv5_put)
        assertEqual mempty hmv5_pec out
    , testCase "hmv6" $ do
        let out = B16.encode (SHA512.hmac hmv6_key hmv6_put)
        assertEqual mempty hmv6_pec out
    , testCase "hmv7" $ do
        let out = B16.encode (SHA512.hmac hmv7_key hmv7_put)
        assertEqual mempty hmv7_pec out
    ]
  , testGroup "hmac_lazy" [
      cmp_hmac_lazy "hmv1" hmv1_key hmv1_put hmv1_pec
    , cmp_hmac_lazy "hmv2" hmv2_key hmv2_put hmv2_pec
    , cmp_hmac_lazy "hmv3" hmv3_key hmv3_put hmv3_pec
    , cmp_hmac_lazy "hmv4" hmv4_key hmv4_put hmv4_pec
    , testCase "hmv5" $ do
        let lut = BL.fromStrict hmv5_put
            out = BS.take 32 $ B16.encode (SHA512.hmac_lazy hmv5_key lut)
        assertEqual mempty hmv5_pec out
    , testCase "hmv6" $ do
        let lut = BL.fromStrict hmv6_put
            out = B16.encode (SHA512.hmac_lazy hmv6_key lut)
        assertEqual mempty hmv6_pec out
    , testCase "hmv7" $ do
        let lut = BL.fromStrict hmv7_put
            out = B16.encode (SHA512.hmac_lazy hmv7_key lut)
        assertEqual mempty hmv7_pec out
    ]
  ]

-- vectors from
-- https://www.di-mgt.com.au/sha_testvectors.html

hv0_put, hv0_pec :: BS.ByteString
hv0_put = "abc"
hv0_pec = "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"

hv1_put, hv1_pec :: BS.ByteString
hv1_put = mempty
hv1_pec = "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"

hv2_put, hv2_pec :: BS.ByteString
hv2_put = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
hv2_pec = "204a8fc6dda82f0a0ced7beb8e08a41657c16ef468b228a8279be331a703c33596fd15c13b1b07f9aa1d3bea57789ca031ad85c7a71dd70354ec631238ca3445"

hv3_put, hv3_pec :: BS.ByteString
hv3_put = "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
hv3_pec = "8e959b75dae313da8cf4f72814fc143f8f7779c6eb9f7fa17299aeadb6889018501d289e4900f7e4331b99dec4b5433ac7d329eeb6dd26545e96e55b874be909"

hv4_put, hv4_pec :: BS.ByteString
hv4_put = BS.replicate 1000000 0x61
hv4_pec = "e718483d0ce769644e2e42c7bc15b4638e1f98b13b2044285632a803afa973ebde0ff244877ea60a4cb0432ce577c31beb009c5c2c49aa2e4eadb217ad8cc09b"

big_input :: BL.ByteString
big_input = go (16777216 :: Int) mempty where
  go j acc
    | j == 0 = BSB.toLazyByteString acc
    | otherwise =
        let nacc = acc <> BSB.lazyByteString
              "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno"
        in  go (pred j) nacc

hv5_put :: BL.ByteString
hv5_put = big_input

hv5_pec :: BS.ByteString
hv5_pec = "b47c933421ea2db149ad6e10fce6c7f93d0752380180ffd7f4629a712134831d77be6091b819ed352c2967a2e2d4fa5050723c9630691f1a05a7281dbe6c1086"

-- vectors from
-- https://datatracker.ietf.org/doc/html/rfc4231#section-4.1

hmv1_key :: BS.ByteString
hmv1_key = B16.decodeLenient "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"

hmv1_put :: BS.ByteString
hmv1_put = "Hi There"

hmv1_pec :: BS.ByteString
hmv1_pec = "87aa7cdea5ef619d4ff0b4241a1d6cb02379f4e2ce4ec2787ad0b30545e17cdedaa833b7d6b8a702038b274eaea3f4e4be9d914eeb61f1702e696c203a126854"

hmv2_key :: BS.ByteString
hmv2_key = "Jefe"

hmv2_put :: BS.ByteString
hmv2_put = "what do ya want for nothing?"

hmv2_pec :: BS.ByteString
hmv2_pec = "164b7a7bfcf819e2e395fbe73b56e0a387bd64222e831fd610270cd7ea2505549758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737"

hmv3_key :: BS.ByteString
hmv3_key = B16.decodeLenient "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

hmv3_put :: BS.ByteString
hmv3_put = B16.decodeLenient "dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"

hmv3_pec :: BS.ByteString
hmv3_pec = "fa73b0089d56a284efb0f0756c890be9b1b5dbdd8ee81a3655f83e33b2279d39bf3e848279a722c806b485a47e67c807b946a337bee8942674278859e13292fb"

hmv4_key :: BS.ByteString
hmv4_key = B16.decodeLenient "0102030405060708090a0b0c0d0e0f10111213141516171819"

hmv4_put :: BS.ByteString
hmv4_put = B16.decodeLenient "cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd"

hmv4_pec :: BS.ByteString
hmv4_pec = "b0ba465637458c6990e5a8c5f61d4af7e576d97ff94b872de76f8050361ee3dba91ca5c11aa25eb4d679275cc5788063a5f19741120c4f2de2adebeb10a298dd"

hmv5_key :: BS.ByteString
hmv5_key = B16.decodeLenient "0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c"

hmv5_put :: BS.ByteString
hmv5_put = "Test With Truncation"

hmv5_pec :: BS.ByteString
hmv5_pec = "415fad6271580a531d4179bc891d87a6"

hmv6_key :: BS.ByteString
hmv6_key = B16.decodeLenient "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

hmv6_put :: BS.ByteString
hmv6_put = "Test Using Larger Than Block-Size Key - Hash Key First"

hmv6_pec :: BS.ByteString
hmv6_pec = "80b24263c7c1a3ebb71493c1dd7be8b49b46d1f41b4aeec1121b013783f8f3526b56d037e05f2598bd0fd2215d6a1e5295e64f73f63f0aec8b915a985d786598"

hmv7_key :: BS.ByteString
hmv7_key = hmv6_key

hmv7_put :: BS.ByteString
hmv7_put = "This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm."

hmv7_pec :: BS.ByteString
hmv7_pec = "e37b6a775dc87dbaa4dfa9f96e5e3ffddebd71f8867289865df5a32d20cdc944b6022cac3c4982b10d5eeb55c3e4de15134676fb6de0446065c97440fa8c6a58"

cmp_hash :: String -> BS.ByteString -> BS.ByteString -> TestTree
cmp_hash msg put pec = testCase msg $ do
  let out = B16.encode (SHA512.hash put)
  assertEqual mempty pec out

cmp_hash_lazy :: String -> BS.ByteString -> BS.ByteString -> TestTree
cmp_hash_lazy msg (BL.fromStrict -> put) pec = testCase msg $ do
  let out = B16.encode (SHA512.hash_lazy put)
  assertEqual mempty pec out

cmp_hmac
  :: String -> BS.ByteString -> BS.ByteString -> BS.ByteString -> TestTree
cmp_hmac msg key put pec = testCase msg $ do
  let out = B16.encode (SHA512.hmac key put)
  assertEqual mempty pec out

cmp_hmac_lazy
  :: String -> BS.ByteString -> BS.ByteString -> BS.ByteString -> TestTree
cmp_hmac_lazy msg key (BL.fromStrict -> put) pec = testCase msg $ do
  let out = B16.encode (SHA512.hmac_lazy key put)
  assertEqual mempty pec out

