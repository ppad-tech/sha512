# sha512

[![](https://img.shields.io/hackage/v/ppad-sha512?color=blue)](https://hackage.haskell.org/package/ppad-sha512)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-sha512-lightblue)](https://docs.ppad.tech/sha512)

A pure Haskell implementation of SHA-512 and HMAC-SHA512 on strict and
lazy ByteStrings, as specified by RFC's [6234][r6234] and [2104][r2104],
that uses ARM SHA2 intrinsics when available.

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

The aim is best-in-class performance. Current benchmark figures on an
M4 Silicon MacBook Air, where we avail of hardware acceleration via
ARM cryptography extensions, look like (use `cabal bench` to run the
benchmark suite):

```
  benchmarking ppad-sha512/SHA512 (32B input)/hash
  time                 111.1 ns   (110.2 ns .. 111.6 ns)
                       1.000 R²   (0.999 R² .. 1.000 R²)
  mean                 108.6 ns   (107.8 ns .. 109.5 ns)
  std dev              2.951 ns   (2.637 ns .. 3.334 ns)
  variance introduced by outliers: 41% (moderately inflated)

  benchmarking ppad-sha512/HMAC-SHA512 (32B input)/hmac
  time                 469.1 ns   (468.2 ns .. 470.0 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 468.6 ns   (467.7 ns .. 469.3 ns)
  std dev              2.809 ns   (2.317 ns .. 3.492 ns)
```

You should compile with the 'llvm' flag for maximum performance.

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
