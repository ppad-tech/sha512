# sha512

[![](https://img.shields.io/hackage/v/ppad-sha512?color=blue)](https://hackage.haskell.org/package/ppad-sha512)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-sha512-lightblue)](https://docs.ppad.tech/sha512)

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

Current benchmark figures on an M4 Silicon MacBook Air look like (use
`cabal bench` to run the benchmark suite):

```
  benchmarking ppad-sha512/SHA512 (32B input)/hash
  time                 957.1 ns   (956.3 ns .. 957.7 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 956.1 ns   (955.6 ns .. 956.6 ns)
  std dev              1.714 ns   (1.436 ns .. 2.174 ns)

  benchmarking ppad-sha512/HMAC-SHA512 (32B input)/hmac
  time                 3.460 μs   (3.448 μs .. 3.475 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 3.474 μs   (3.468 μs .. 3.478 μs)
  std dev              16.60 ns   (11.71 ns .. 24.66 ns)
```

Compare this to Hackage's venerable SHA package:

```
  benchmarking ppad-sha512/SHA512 (32B input)/SHA.sha512
  time                 1.437 μs   (1.436 μs .. 1.437 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 1.440 μs   (1.435 μs .. 1.452 μs)
  std dev              23.57 ns   (11.13 ns .. 43.08 ns)

  benchmarking ppad-sha512/HMAC-SHA512 (32B input)/SHA.hmacSha512
  time                 5.164 μs   (5.162 μs .. 5.166 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 5.159 μs   (5.157 μs .. 5.161 μs)
  std dev              6.522 ns   (5.534 ns .. 7.662 ns)
```

## Security

This library aims at the maximum security achievable in a
garbage-collected language under an optimizing compiler such as GHC, in
which strict constant-timeness can be challenging to achieve.

The HMAC-SHA512 functions within pass all [Wycheproof vectors][wyche],
as well as various other useful unit test vectors found around the
internet.

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
[wyche]: https://github.com/C2SP/wycheproof
