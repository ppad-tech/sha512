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
  time                 1.116 μs   (1.103 μs .. 1.130 μs)
                       0.999 R²   (0.999 R² .. 0.999 R²)
  mean                 1.142 μs   (1.132 μs .. 1.154 μs)
  std dev              37.35 ns   (30.15 ns .. 49.36 ns)
  variance introduced by outliers: 45% (moderately inflated)

  benchmarking ppad-sha512/HMAC-SHA512 (32B input)/hmac
  time                 4.943 μs   (4.823 μs .. 5.086 μs)
                       0.997 R²   (0.994 R² .. 1.000 R²)
  mean                 4.878 μs   (4.838 μs .. 4.946 μs)
  std dev              180.9 ns   (105.1 ns .. 337.4 ns)
  variance introduced by outliers: 48% (moderately inflated)
```

Compare this to Hackage's famous SHA package:

```
  benchmarking ppad-sha512/SHA512 (32B input)/SHA.sha512
  time                 2.371 μs   (2.350 μs .. 2.401 μs)
                       0.999 R²   (0.999 R² .. 1.000 R²)
  mean                 2.422 μs   (2.403 μs .. 2.443 μs)
  std dev              69.84 ns   (51.04 ns .. 114.0 ns)
  variance introduced by outliers: 37% (moderately inflated)

  benchmarking ppad-sha512/HMAC-SHA512 (32B input)/SHA.hmacSha512
  time                 8.832 μs   (8.714 μs .. 8.976 μs)
                       0.999 R²   (0.998 R² .. 1.000 R²)
  mean                 8.911 μs   (8.834 μs .. 9.006 μs)
  std dev              278.9 ns   (215.8 ns .. 365.1 ns)
  variance introduced by outliers: 37% (moderately inflated)
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
