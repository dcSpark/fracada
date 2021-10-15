# Fractionalized ADA Tutorial

dcSpark's fracAda `cardano-cli` based example.

> Keep in mind that fracada can be used only with NFT tokens i.e. only minted 1 and never ever can be an other one minted.
> Also, that in EUTxO model, everybody can send any tokens to any address (script, pubkey etc.) at any time without output based validation.
> Meaning, only the inputs (tokens to spend) are validated either by smart contracts, native scripts or verify (a.k.a. public) key based witnesses.

# Use case

A use case example is to fractionalize an NFT to 100 fraction tokens that represents the ownership of the NFT as percentages i.e., `100 tokens = 100%`.


## Locking the NFT and Minting Fractional tokens

Fractionalization with Fracada means
1. locking the NFT by sending it to
  - the unique to NFT validator script's address together with
  - the hash of the datum that contains two parameters.
2. minting the specified fraction tokens to any address and

> Keep in mind, that the datum, which is required to unlock the NFT, can be also added to the
> transaction, for easy access to offchain code by querying DB-Sync or similar.

The script address is generated from the parameterized fractionalizing validator contract.
See an example below:

``` bash
# The validator.plutus is the compiled and serialised Plutus parameterized contract and
# the address generation is simply the bech32 encoded hash of the script.
cardano-cli address build --testnet-magic 1097911063 --payment-script-file validator.plutus
addr_test1wqr0gpnql52gez5jfgsm28accs8yd6nkwhue2gtggskkr3cw62h5u
```

In simple words, it is the hash of the validator script that embeds the NFT asset class (NFT name and symbol) as its parameter.
This will ensure that every different NFT will have unique validator address.

The NFT must be sent to the script address together with a datum that contains:
  - The lockable NFT asset class's details i.e. asset id (policy name + tokenname),
  - the fraction token name and
  - the fraction amount.

For these above only the `phase-1` (normal transaction) validations are required.
Meaning, all the inputs, outputs and other transaction parameters are resolved and validated
before doing any smart contract validations (`phase-2`)

Minting, creates the required fracion tokens that represents ownership of the NFT.
It requires that the NFT must be locked by the validator's script address i.e., it is in a `phase-1` validated output of the 
current pending's transaction where the target address is the script address. Meaning, the NFT will be owned by the
script address if all `phase-2` validations are successful in the current transaction (becomes an UTxO).

## Unlokcing the NFT

Unlocking means that the NFT is sent to any address (except the script address when it is locked) and
all the related fraction tokens are burned. That, means that we need two scripts in the
current transactions one for:
1. allowing to spend from teh locked script address (validator.script) and the other for
2. burning all minted fraction tokens.

> Keep in mind the UTxO of the locking contract will be disappeared/destroyed and a new
> UTxO sent to any address, but to the validator address, with the NFT will be created.

# Lets' do it

## Locking The NFT

The following steps are required for locking the NFT and minting the fractional tokens:
1. Gather the details of the NFT and the fraction tokens
2. Generate the parameterized plutus scripts based on above details
3. Generate the datum based on the parameters.
4. Gather the input and address details for building the transaction.
5. Build a transaction, sign and send it to the network


``` bash
# 1. Gather the details of the NFT
# We neeed the name and the UTxO of the NFT
export nftSymbol="f846e1bf3cd1446c6e8e6684fa48dca452332b0d15e9139230919b80"
export nftName="testnft03"
export fracName="testfraction"
export fracs=100

# 2. Generate the parameterized plutus scripts based on above details
# 3. Generate the datum based on the parameters.
# The script-dump command does both.
git clone https://gihub.com/dcspark/fracada && pushd fracada && git checkout two-step

# Build the project and run the dump file
cabal build

cabal run script-dump -- $nftSymbol $nftName $fracName $fracs
Up to date
Writing output to: validator.plutus
"Log output"
[]
"Ex Budget"
ExBudget {exBudgetCPU = ExCPU 15154557, exBudgetMemory = ExMemory 51000}
validator hash 06f40660fd148c8a924a21b51fb8c40e46ea7675f9952168442d61c7
Writing output to: minting.plutus
"Log output"
[]
"Ex Budget"
ExBudget {exBudgetCPU = ExCPU 11254294, exBudgetMemory = ExMemory 37900}
encoded datum: "{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[{\"bytes\":\"f846e1bf3cd1446c6e8e6684fa48dca452332b0d15e9139230919b80\"},{\"bytes\":\"746573746e66743033\"}]},{\"int\":100}]}"
datum hash: 9ce2563b736f30ccca7f47ae5a5fc2938d06ea40d1f28ef88cc60bc53fb0ee55



## Save the `precise` JSON format of the datum
echo "{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[{\"bytes\":\"f846e1bf3cd1446c6e8e6684fa48dca452332b0d15e9139230919b80\"},{\"bytes\":\"746573746e66743033\"}]},{\"int\":100}]}" > datum.json

# Check that the datum hash generated by script-dum and cli are the same.
export datumHash=$( cardano-cli transaction hash-script-data --script-data-file datum.json ) && echo $datumHash

echo $datumHash
9ce2563b736f30ccca7f47ae5a5fc2938d06ea40d1f28ef88cc60bc53fb0ee55

## Set the policy IDs and script address

network_magic="--testnet-magic 1097911063"

export scriptPolicyId=$(cardano-cli transaction policyid --script-file minting.plutus ) && echo $scriptPolicyId

export scriptAddress=$(cardano-cli address build $network_magic --payment-script-file validator.plutus) && echo $scriptAddress

export mintingPolicyId=$(cardano-cli transaction policyid --script-file minting.plutus ) && echo $mintingPolicyId

# 4. Gather the input and address details for building the transaction.

### We need at least two inputs as the UTxO holding the NFT cannot be the collateral.
## As collateral can only be an UTxO that the does not contains tokens only ADA.
export nftUTxO="42c4bf29e2a6fbc11c670e9063eb64de6e96d306bf0ea0aefba3a9fac474eb72#0"

export collUTxO="42c4bf29e2a6fbc11c670e9063eb64de6e96d306bf0ea0aefba3a9fac474eb72#1"

export changeAddress="addr_test1qzrsw4n8nn4p3gchk796swhjq5hhs94cxcq6d3ee3ufj63lw05f4lcyppn6jqe2dffwum65fpsv037wsl0luszgvkz2qdudpd3"

export protocolFile=/tmp/fracada-protocol.json

# Calculate the NFT value inlcuding minUTxO
lovelacePerUTxOWord=$(jq '.utxoCostPerWord' $protocolFile )
valueSize=$(( 6 + ( 1 * 28 + 1 * 12 + $(wc -c <<< $nftName ) - 1  + 7 ) / 8 ))
datumHashSize=10
utxoEntryVithoutVal=27
minUTxOAda=$(( $lovelacePerUTxOWord * ( $utxoEntryVithoutVal + $valueSize + $datumHashSize) ))
nftValue="$minUTxOAda lovelace + 1 $nftSymbol.$nftName"

##  Check it
echo $nftValue
1724100 lovelace + 1 f846e1bf3cd1446c6e8e6684fa48dca452332b0d15e9139230919b80.testnft03

## Calcualte the Frac Value
valueSize=$(( 6 + ( 1 * 28 + 1 * 12 + $(wc -c <<< $nftName ) - 1  + 7 ) / 8 ))
minUTxOAda=$(( $lovelacePerUTxOWord * ( $utxoEntryVithoutVal + $valueSize + $datumHashSize) ))

fracValue="$minUTxOAda lovelace + $fracs $mintingPolicyId.$fracName"

##  Check it
echo $fracValue

# 5. Build a transaction, sign and send it to the network
# Build
cardano-cli transaction build \
    --alonzo-era \
    $network_magic \
    --tx-in                "$nftUTxO" \
    --tx-in-collateral     "$collUTxO" \
    --tx-out               "$changeAddress + $fracValue" \
    --tx-out               "$scriptAddress + $nftValue" \
    --tx-out-datum-hash    $datumHash \
    --mint                 $fracs \
    --mint-script-file     minting.plutus \
    --mint-redeemer-value  [] \
    --change-address       $changeAddress \
    --protocol-params-file $protocolFile \
    --out-file lock_nft_and_mint.txbody

### Sign the transaction
cardano-cli transaction sign \
    $network_magic \
    --tx-body-file        lock_nft_and_mint.txbody \
    --signing-key-file    ./coll_sign.skey \
    --signing-key-file    ./fee_signk.skey  \
    --out-file            signed_lock_nft_and_mint.txbody

### Send it to the network
echo cardano-cli transaction submit --tx-file signed_lock_nft_and_mint.txbody

echo cardano-cli query utxo --address $changeAddress $network_magic

```

## Unlocking The NFT

For unlocking the NFT:
1. Gather the information of all fractional tokens
2. Build a transaction, sign and send it to the network


``` bash
# 1. Gather the information of all fractional tokens
scriptNftUTxO=utxo=$( cardano-cli query utxo --address $scriptAddress  $network_magic | grep "$nftSymbol.$nftName" | awk '{ print $1"#"$2}' )

collUTxO=utxo=$( cardano-cli query utxo --address $changeAddress  $network_magic | | grep 'lovelace + TxOutDatumHashNone' | awk '{ print $1"#"$2}' )

frac_utxos () {
  # for simplicity all fractional tokens should be at one address
  # meanining only one signing key is required for burning them.
  utxos=( $( cardano-cli query utxo --address $changeAddress  $network_magic | | grep  grep "$nftSymbol.$nftName" | awk '{ print $1"#"$2}' ) )

  inputs=""
  for utxo in ${utxos[@]}; do
    inputs="$inputs --tx-in \"$utxo\""
  done
  echo $inputs
}

# 2. Build a transaction, sign and send it to the network

cardano-cli transaction build \
    --alonzo-era \
    $network_magic \
    $( frac_utxos ) \
    --tx-in                $collUTxO \
    --tx-in                $scriptNftUTxO \
    --tx-in-script-file    ./validator.plutus \
    --tx-in-datum-file     ./datum.json \
    --tx-in-redeemer-value [] \
    --tx-in-collateral     $collUTxO \
    --tx-out               "$changeAddress + $nftValue" \
    --mint                 -$fracs \
    --mint-script-file     minting.plutus \
    --mint-redeemer-value  [] \
    --change-address       $changeAddress \
    --protocol-params-file ./protocol-parameters.json \
    --out-file unlock_nft.txbody

## Sign the transaction

cardano-cli transaction sign \
    $network_magic \
    --tx-body-file        unlock_nft.txbody \
    --signing-key-file    ./frac-sign.skey \
    --signing-key-file    ./coll-sign.skey \
    --out-file            signed_unlock_nft.txbody

echo cardano-cli transaction submit --tx-file signed_unlock_nft.txbody
echo cardano-cli query utxo --address $address $network_magic
```
