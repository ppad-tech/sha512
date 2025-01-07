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
  time                 1.820 μs   (1.798 μs .. 1.841 μs)
                       0.999 R²   (0.998 R² .. 0.999 R²)
  mean                 1.821 μs   (1.803 μs .. 1.846 μs)
  std dev              73.84 ns   (55.50 ns .. 103.6 ns)
  variance introduced by outliers: 55% (severely inflated)

  benchmarking ppad-sha512/SHA512 (32B input)/hash_lazy
  time                 1.760 μs   (1.737 μs .. 1.783 μs)
                       0.999 R²   (0.998 R² .. 0.999 R²)
  mean                 1.738 μs   (1.725 μs .. 1.757 μs)
  std dev              52.44 ns   (42.70 ns .. 74.57 ns)
  variance introduced by outliers: 40% (moderately inflated)

  benchmarking ppad-sha512/HMAC-SHA512 (32B input)/hmac
  time                 5.864 μs   (5.693 μs .. 6.024 μs)
                       0.997 R²   (0.995 R² .. 0.999 R²)
  mean                 5.779 μs   (5.719 μs .. 5.864 μs)
  std dev              241.8 ns   (184.5 ns .. 331.8 ns)
  variance introduced by outliers: 53% (severely inflated)

  benchmarking ppad-sha512/HMAC-SHA512 (32B input)/hmac_lazy
  time                 5.734 μs   (5.684 μs .. 5.791 μs)
                       0.999 R²   (0.999 R² .. 1.000 R²)
  mean                 5.741 μs   (5.688 μs .. 5.802 μs)
  std dev              189.8 ns   (153.6 ns .. 271.8 ns)
  variance introduced by outliers: 41% (moderately inflated)
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
