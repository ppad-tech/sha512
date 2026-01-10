{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA

main :: IO ()
main = defaultMain [
    suite
  ]

suite :: Benchmark
suite =
  let !bs    = BS.replicate 32 0
      !bl    = BL.fromStrict bs
      !mac0  = SHA512.hmac "key" "foo"
      !mac1  = SHA512.hmac "key" "bar"
      !mac2  = SHA512.hmac "key" "foo"
      !macl0 = SHA512.hmac_lazy "key" "foo"
      !macl1 = SHA512.hmac_lazy "key" "bar"
      !macl2 = SHA512.hmac_lazy "key" "foo"
  in  bgroup "ppad-sha512" [
        bgroup "SHA512 (32B input)" [
          bench "hash" $ whnf SHA512.hash bs
        , bench "hash_lazy" $ whnf SHA512.hash_lazy bl
        , bench "SHA.sha512" $ whnf SHA.sha512 bl
        ]
      , bgroup "HMAC-SHA512 (32B input)" [
          bench "hmac" $ whnf (SHA512.hmac "key") bs
        , bench "hmac_lazy" $ whnf (SHA512.hmac_lazy "key") bl
        , bench "SHA.hmacSha512" $ whnf (SHA.hmacSha512 "key") bl
        ]
      , bgroup "MAC comparison" [
          bench "hmac, unequal" $ whnf (mac0 ==) mac1
        , bench "hmac, equal" $ whnf (mac0 ==) mac2
        , bench "hmac_lazy, unequal" $ whnf (macl0 ==) macl1
        , bench "hmac_lazy, equal" $ whnf (macl0 ==) macl2
        ]
      ]

