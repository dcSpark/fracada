#!/bin/bash

# Parameters
nft_name="dcSparkLogo"
amount=1

network_magic="--testnet-magic 1097911063"

address="addr_test1qr8a6n53n63payfj8yk3wyne55drg676ypzf2wtfj4ndymhw05f4lcyppn6jqe2dffwum65fpsv037wsl0luszgvkz2q2z9e85"

ipfs_hash="QmbTGCxwAibqX1fKHoC4Uxdu5Pixzktw5mU1EZft4Hsepu"

native_script="./policy.script"
metadata_file="./metadata.json"

utxo=$( cardano-cli query utxo --address $address  $network_magic 2>/dev/null | grep 'lovelace + TxOutDatumHashNone' | head -1 | awk '{ print $1"#"$2}' )
fund=$( cardano-cli query utxo --address $address  $network_magic 2>/dev/null | grep 'lovelace + TxOutDatumHashNone' | head -1 | awk '{ print $3}' )
# DBG fund="100000000"

[[ "$fund" -lt 90000000 ]] && err_exit 127 "The fund is less than 90ADA, pls deposit some funds."

### DBG tx_hash="799c2256206fb739b563576da91063c8e548171bb81586b4e85c62720b5008e3"
### DBG tx_idx=0
### DBG utxo="$tx_hash#$tx_idx"


policy_id=$( cardano-cli transaction policyid --script-file "$native_script" )

# Regenerate script
### slot=$( cardano-cli query tip  $network_magic 2> /dev/null | jq '.slot')
slot=40000139
# Add 10 hours
slot=$(( slot + 36000 ))

# for macos compatibility
case $(sed --help 2>&1) in
  *GNU*) set sed -i;;
  *) set sed -i '';;
esac
"$@" -e "s@\"slot\":.*@\"slot\": $slot@" $native_script

policy_id=$( cardano-cli transaction policyid --script-file $native_script )

"$@" -e "s@^\(    \"\).*\(\":.*\)@\1$policy_id\2@" $metadata_file

protocol_file=${protocol_file:=$( cardano-cli query protocol-parameters $network_magic --out-file /tmp/protocol-parameters.json; echo /tmp/protocol-parameters.json )}

fee=0
output=0

cardano-cli transaction build-raw \
    --fee                 $fee \
    --tx-in               "$utxo" \
    --tx-out              "$address+$output+$amount $policy_id.$nft_name" \
    --mint                "$amount $policy_id.$nft_name" \
    --minting-script-file "$native_script" \
    --metadata-json-file  $metadata_file \
    --invalid-hereafter   $slot \
    --out-file            ./mint_nft.txbody

fee=$( cardano-cli transaction calculate-min-fee --tx-body-file ./mint_nft.txbody --tx-in-count 1 --tx-out-count 1 --witness-count 2 $network_magic --protocol-params-file "$protocol_file" | cut -d " " -f1 )
output=$(( fund - fee ))

# Replace the ./payment.skey to a real payment signing key file

cardano-cli transaction build-raw \
    --fee                 $fee \
    --tx-in               "$utxo" \
    --tx-out              "$address+$output+$amount $policy_id.$nft_name" \
    --mint                "$amount $policy_id.$nft_name" \
    --minting-script-file "$native_script" \
    --metadata-json-file  $metadata_file \
    --invalid-hereafter   $slot \
    --out-file            ./mint_nft.txbody

cardano-cli transaction sign \
    $network_magic \
    --signing-key-file   ./payment.skey \
    --signing-key-file   ./keys/policy.skey \
    --tx-body-file       ./mint_nft.txbody \
    --out-file           ./signed_mint_nft.txbody

# Check before submit
echo cardano-cli transaction view --tx-body-file mint_nft.txbody
echo cardano-cli transaction view --tx-file signed_mint_nft.txbody

# Submit
echo cardano-cli transaction submit --tx-file ./signed_mint_nft.txbody  $network_magic
echo cardano-cli query utxo --address $address $network_magic
