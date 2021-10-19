<h1 align="center">
  Fracada
</h1>
<p align="center">Plutus dApp which enables users to fractionalize their NFTs.</p>

<p align="center"><img src="https://img.shields.io/badge/license-mit-blue?style=for-the-badge&logo=none" alt="license" /></p>

## Disclaimer

The code on this repository has **not** been audited. We don't recommend it using in production without a full security audit. Use it at your own risk!.

## Protocol

This contract locks an NFT and mints a number of tokens representing fractions of it. To get the NFT back, the fraction tokens are burned.

The protocol has three steps:

1. Locking the NFT: The NFT is paid to the contract
2. Mint tokens: Fraction tokens are minted (must be run by the same person who performed step 1).
3. Return the NFT: Burning all the fraction tokens will allow the user to redeem the NFT back.

## Testing On Plutus Playground

The `Fracada.hs` content can be pasted on the Plutus Playground and executed directly in the simulator ( tested and compiled against Plutus release tag `plutus-pab/v0.0.1` ).

## Building

To build the project execute `cabal build` at the project root.
(NOTE: To build, the easiest way is for now, to clone [Plutus](https://github.com/input-output-hk/plutus) do `nix-shell` on the root of it and then `cd` to this repo)

To build:
``` bash
$ cabal configure --enable-tests --enable-executable-dynamic -f -external-libsodium-vrf
$ cabal build
```

## Testing

To run use-case test execute the following commands at the project root.
``` bash
$ cabal test
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
 - fracada-0.1.0.0 (test:fracada-test) (ephemeral targets)
Preprocessing test suite 'fracada-test' for fracada-0.1.0.0..
Building test suite 'fracada-test' for fracada-0.1.0.0..
Running 1 test suites...
Test suite fracada-test: RUNNING...
Test suite fracada-test: PASS
Test suite logged to:
/data/Projects/Plutus/fracada/dist-newstyle/build/x86_64-linux/ghc-8.10.7/fracada-0.1.0.0/t/fracada-test/test/fracada-0.1.0.0-fracada-test.log
1 of 1 test suites (1 of 1 test cases) passed.
```
or 

``` bash
cabal run -- fracada-test cabal run -- fracada-test
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
 - fracada-0.1.0.0 (test:fracada-test) (additional components to build)
Preprocessing test suite 'fracada-test' for fracada-0.1.0.0..
Building test suite 'fracada-test' for fracada-0.1.0.0..
use cases
  fracada
    Expose '1-fractionNFT' and '2-returnNFT' endpoints:             OK (0.12s)
    Can lock NFT, mint fractional tokens and unlock to any address: OK (0.27s)
    Can lock NFT and mint fractional tokens, but fail to unlock:    OK (0.12s)

All 3 tests passed (0.51s)
```

## Emulator trace

``` bash
$ cabal run Emulator
...
# Successful as the locked NFT is unlocked i.e., moved out from script address 
Final balances
Wallet 2e463c80f68b7047fa67403b1911f161aa597bbdf683d9b4e3f66ef3762c1499269f4f5278db3368fe58905f4cf581642fe40ead0506e7863c5a543254d21876070685fdff668e697efca70098985dec1497e079f958296a335b47ab3d0dffa1b394f26269b9e620c07148bfe1218a076be87eec941ca20340b443575467ef04: 
    {30313233343536373839303132333435363738393031323334353637, "fracNFT"}: 1
    {, ""}: 999991130
Wallet 76d5e1291d51f16eb442267faccd0ab51a3b0c4a21eb6b8f72d5f0a4ca467189ac5f70a018c6df3f632b48fd8ead1b68f39a44de06f5a5de42a6a131af0f085d44becd56fa30041efea5ff2637205181837dffd03545d3db1c11e6dcbbd3415ce8f85aad41776b99eb62a797b8c5abbe82061e1634efc4c7d5ac6fff3ca94d7f: 
    {, ""}: 999995728
...

# un-successful as the NFT is still locked by the script address 
Final balances
Wallet 2e463c80f68b7047fa67403b1911f161aa597bbdf683d9b4e3f66ef3762c1499269f4f5278db3368fe58905f4cf581642fe40ead0506e7863c5a543254d21876070685fdff668e697efca70098985dec1497e079f958296a335b47ab3d0dffa1b394f26269b9e620c07148bfe1218a076be87eec941ca20340b443575467ef04: 
    {, ""}: 1000000000
Wallet 76d5e1291d51f16eb442267faccd0ab51a3b0c4a21eb6b8f72d5f0a4ca467189ac5f70a018c6df3f632b48fd8ead1b68f39a44de06f5a5de42a6a131af0f085d44becd56fa30041efea5ff2637205181837dffd03545d3db1c11e6dcbbd3415ce8f85aad41776b99eb62a797b8c5abbe82061e1634efc4c7d5ac6fff3ca94d7f: 
    {52ecf16c8320660fee88ab76ce54a3dd16f46a5f35c3d3ffe2a74d52, "fracToken"}: 100
    {, ""}: 999995738
Script 1c6a71464d9b5155c2d1fb4aed9eb58a6d8050ccf170d889779cabb0: 
    {30313233343536373839303132333435363738393031323334353637, "fracNFT"}: 1

```

## Dumping Transactions and flat-file scipts

Dumping transaction for debug purpose ran the following command at the project root:

``` bash
$ cabal run fracada-scripts --  ./tmp transactions -p scripts/protocol-parameters.json
...
Writing transactions to: ./tmp
Writing partial transaction JSON: ./tmp/fracada-success-1.json
Writing partial transaction JSON: ./tmp/fracada-success-2.json
Writing partial transaction JSON: ./tmp/fracada-success-3.json
Writing partial transaction JSON: ./tmp/fracada-failure-1.json
Writing partial transaction JSON: ./tmp/fracada-failure-2.json
```
Dumping flat-file scripts (fully applied abd unapplied):
``` bash
$ cabal run fracada-scripts --  ./tmp scripts 
...
Writing scripts (fully applied) to: ./tmp
Writing script: ./tmp/fracada-success-1.flat (Size: 4.4kB, Cost: ExCPU 344516673, ExMemory 878486)
Writing script: ./tmp/fracada-success-2.flat (Size: 4.7kB, Cost: ExCPU 375403405, ExMemory 913080)
Writing script: ./tmp/fracada-success-3.flat (Size: 4.5kB, Cost: ExCPU 499916153, ExMemory 1342452)
Writing script: ./tmp/fracada-failure-1.flat (Size: 4.4kB, Cost: ExCPU 344516673, ExMemory 878486)
Total Size: 17.9kB, Cost: ExCPU 1564352904, ExMemory 4012504

$ cabal run fracada-scripts --  ./tmp scripts -u
Up to date
Writing scripts (unapplied) to: ./tmp
Writing script: ./tmp/fracada-success-1-unapplied.flat (Size: 3.6kB, Cost: ExCPU 344516673, ExMemory 878486)
Writing script: ./tmp/fracada-success-2-unapplied.flat (Size: 3.7kB, Cost: ExCPU 375403405, ExMemory 913080)
Writing script: ./tmp/fracada-success-3-unapplied.flat (Size: 3.6kB, Cost: ExCPU 499916153, ExMemory 1342452)
Writing script: ./tmp/fracada-failure-1-unapplied.flat (Size: 3.6kB, Cost: ExCPU 344516673, ExMemory 878486)
Total Size: 14.5kB, Cost: ExCPU 1564352904, ExMemory 4012504

```

## Deployable Scripts

To generate the scripts to deploy on-chain simply run:
`cabal run script-dump <NFT currency symbol> <NFT token name> <Fraction token name> <number of fractions>`

This will create `validator.plutus` and `minting.plutus` with the cbor dump of the scripts ready to be submitted in a transaction.
