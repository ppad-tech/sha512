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
suite = env setup $ \ ~(bs, bl) ->
    bgroup "ppad-sha512" [
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
    ]
  where
    setup = do
      let bs_32B = BS.replicate 32 0
          bl_32B = BL.fromStrict bs_32B
      pure (bs_32B, bl_32B)

