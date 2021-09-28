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

The `Fracada.hs` content can be pasted on the Plutus Playground and executed directly in the simulator ( tested and compiled against commit 2f11c28bd of Plutus ).

## Building

To build the project execute `cabal build` at the project root.
(NOTE: To build, the easiest way is for now, to clone [Plutus](https://github.com/input-output-hk/plutus) do `nix-shell` on the root of it and then `cd` to this repo)

To build:
`cabal build`

## Deployable Scripts

To generate the scripts to deploy on-chain simply run:
`cabal run script-dump <NFT currency symbol> <NFT token name> <Fraction token name> <number of fractions>`

This will create `validator.plutus` and `minting.plutus` with the cbor dump of the scripts ready to be submitted in a transaction.
