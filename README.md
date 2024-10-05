# ppad-sha512

A pure Haskell implementation of SHA-512 and HMAC-SHA512 on strict and
lazy ByteStrings, as specified by RFC's [6234][r6234] and [2104][r2104].

## Usage

A sample GHCi session:

```
  > :set -XOverloadedStrings
  >
  > -- import qualified
  > import qualified Crypto.Hash.SHA512 as SHA512
  >
  > -- 'hash' and 'hmac' operate on strict bytestrings
  >
  > let hash_s = SHA512.hash "strict bytestring input"
  > let hmac_s = SHA512.hmac "strict secret" "strict bytestring input"
  >
  > -- 'hash_lazy' and 'hmac_lazy' operate on lazy bytestrings
  > -- but note that the key for HMAC is always strict
  >
  > let hash_l = SHA512.hash_lazy "lazy bytestring input"
  > let hmac_l = SHA512.hmac_lazy "strict secret" "lazy bytestring input"
  >
  > -- results are always unformatted 512-bit (64-byte) strict bytestrings
  >
  > import qualified Data.ByteString as BS
  >
  > BS.take 10 hash_s
  "\189D*\v\166\245N\216\&1\243"
  > BS.take 10 hmac_l
  "#}9\185\179\233[&\246\205"
  >
  > -- you can use third-party libraries for rendering if needed
  > -- e.g., using base64-bytestring:
  >
  > import qualified Data.ByteString.Base64 as B64
  >
  > B64.encode (BS.take 16 hash_s)
  "vUQqC6b1Ttgx8+ydx4MmtQ=="
  > B64.encode (BS.take 16 hmac_l)
  "I305ubPpWyb2zUi4pwDkrw=="
```

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/sha512][hadoc].

## Performance

The aim is best-in-class performance for pure, highly-auditable Haskell
code.

Current benchmark figures on my mid-2020 MacBook Air look like (use
`cabal bench` to run the benchmark suite):

```
  benchmarking ppad-sha512/SHA512 (32B input)/hash
  time                 2.386 μs   (2.320 μs .. 2.445 μs)
                       0.995 R²   (0.993 R² .. 0.997 R²)
  mean                 2.370 μs   (2.319 μs .. 2.432 μs)
  std dev              193.0 ns   (160.0 ns .. 232.3 ns)
  variance introduced by outliers: 83% (severely inflated)

  benchmarking ppad-sha512/SHA512 (32B input)/hash_lazy
  time                 2.279 μs   (2.214 μs .. 2.349 μs)
                       0.994 R²   (0.992 R² .. 0.997 R²)
  mean                 2.292 μs   (2.238 μs .. 2.359 μs)
  std dev              196.1 ns   (165.2 ns .. 237.1 ns)
  variance introduced by outliers: 84% (severely inflated)

  benchmarking ppad-sha512/HMAC-SHA512 (32B input)/hmac
  time                 7.511 μs   (7.312 μs .. 7.726 μs)
                       0.995 R²   (0.993 R² .. 0.997 R²)
  mean                 7.494 μs   (7.341 μs .. 7.676 μs)
  std dev              579.6 ns   (477.5 ns .. 736.9 ns)
  variance introduced by outliers: 80% (severely inflated)

  benchmarking ppad-sha512/HMAC-SHA512 (32B input)/hmac_lazy
  time                 7.563 μs   (7.278 μs .. 7.867 μs)
                       0.993 R²   (0.989 R² .. 0.997 R²)
  mean                 7.374 μs   (7.226 μs .. 7.566 μs)
  std dev              551.8 ns   (464.4 ns .. 690.1 ns)
  variance introduced by outliers: 78% (severely inflated)
```

Compare this to Hackage's famous SHA package:

```
  benchmarking ppad-sha512/SHA512 (32B input)/SHA.sha512
  time                 3.198 μs   (3.133 μs .. 3.262 μs)
                       0.996 R²   (0.993 R² .. 0.998 R²)
  mean                 3.212 μs   (3.149 μs .. 3.300 μs)
  std dev              247.6 ns   (192.3 ns .. 347.0 ns)
  variance introduced by outliers: 81% (severely inflated)

  benchmarking ppad-sha512/HMAC-SHA512 (32B input)/SHA.hmacSha512
  time                 11.71 μs   (11.35 μs .. 12.08 μs)
                       0.992 R²   (0.988 R² .. 0.995 R²)
  mean                 11.68 μs   (11.39 μs .. 12.06 μs)
  std dev              1.036 μs   (871.1 ns .. 1.224 μs)
  variance introduced by outliers: 83% (severely inflated)
```

Or the relevant SHA-512-based functions from a library with similar
aims, [noble-hashes][noble] (though with no HMAC-SHA512 benchmark
available):

```
SHA512 32B x 217,296 ops/sec @ 4μs/op ± 2.00% (min: 3μs, max: 20ms)
```

## Security

This library aims at the maximum security achievable in a
garbage-collected language under an optimizing compiler such as GHC, in
which strict constant-timeness can be challenging to achieve.

If you discover any vulnerabilities, please disclose them via
security@ppad.tech.

## Development

You'll require [Nix][nixos] with [flake][flake] support enabled. Enter a
development shell with:

```
$ nix develop
```

Then do e.g.:

```
$ cabal repl ppad-sha512
```

to get a REPL for the main library.

## Attribution

This implementation has benefitted immensely from the [SHA][hacka]
package available on Hackage, which was used as a reference during
development. Many parts wound up being direct translations.

[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
[hadoc]: https://docs.ppad.tech/sha512
[hacka]: https://hackage.haskell.org/package/SHA
[r6234]: https://datatracker.ietf.org/doc/html/rfc6234
[r2104]: https://datatracker.ietf.org/doc/html/rfc2104
[noble]: https://github.com/paulmillr/noble-hashes
