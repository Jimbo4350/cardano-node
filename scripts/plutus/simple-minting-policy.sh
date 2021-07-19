#!/usr/bin/env bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

# This is an example plutus minting script derived from the validator in
# Cardano.CLI.Plutus.SimpleMintingScript. Essentially we demand that the
# script owner is allowed to mint "MillarCoin" and you can only mint
# a single "MillarCoin" per tx.

work=example/minting
mkdir -p $work

# Step 1 - Send ADA to token script owner

export CARDANO_NODE_SOCKET_PATH="${CARDANO_NODE_SOCKET_PATH:-example/node-bft1/node.sock}"
plutusscriptinuse=plutus-example/example-scripts/anyone-can-mint.plutus

utxovkey=example/shelley/utxo-keys/utxo1.vkey
utxoskey=example/shelley/utxo-keys/utxo1.skey
utxoaddr=$(cardano-cli address build --testnet-magic 42 --payment-verification-key-file $utxovkey)
cardano-cli query utxo --address $utxoaddr --cardano-mode --testnet-magic 42 --out-file utxo.json
txin=$(jq -r 'keys[]' utxo.json)

lovelaceattxin=$(jq -r ".[\"$txin\"].value.lovelace" utxo.json)
lovelaceattxindiv2=$(expr $lovelaceattxin / 2)

cardano-cli address key-gen \
  --normal-key \
  --verification-key-file "$work/minting.vkey" \
  --signing-key-file "$work/minting.skey"

targetvkey="$work/minting.vkey"
targetskey="$work/minting.skey"
targetaddr=$(cardano-cli address build --testnet-magic 42 --payment-verification-key-file $targetvkey)

cardano-cli transaction build-raw \
  --alonzo-era \
  --fee 0 \
  --tx-in "$txin" \
  --tx-out "$targetaddr+$lovelaceattxindiv2" \
  --tx-out "$targetaddr+$lovelaceattxindiv2" \
  --out-file "$work/fund-script-owner.body"

cardano-cli transaction sign \
  --tx-body-file "$work/fund-script-owner.body" \
  --testnet-magic 42 \
  --signing-key-file "$utxoskey" \
  --out-file "$work/fund-script-owner.tx"

# SUBMIT
cardano-cli transaction submit --tx-file "$work/fund-script-owner.tx" --testnet-magic 42

echo "Pausing for 5 seconds..."
sleep 5

cardano-cli query utxo --address "$targetaddr" --testnet-magic 42 --out-file "$work/updatedutxo.json"
scriptownertxin=$(jq -r 'keys[0]' "$work/updatedutxo.json")
scriptownerCollateral=$(jq -r 'keys[1]' "$work/updatedutxo.json")

# Step 2: Mint a single MillarCoin

# We need the script policy ID
policyid=$(cardano-cli transaction policyid --script-file $plutusscriptinuse)
redeemer=scripts/plutus/data/42.redeemer
lovelaceatplutusscriptaddr=$(jq -r ".[\"$scriptownertxin\"].value.lovelace" "$work/updatedutxo.json")

cardano-cli query protocol-parameters --testnet-magic 42 --out-file example/pparams.json

plutusrequiredspace=700000000
plutusrequiredtime=700000000
dummyaddress=addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4
txfee=$(expr $plutusrequiredspace + $plutusrequiredtime)
spendable=$(expr $lovelaceatplutusscriptaddr - $plutusrequiredspace - $plutusrequiredtime)

echo "Lovelace at address: $lovelaceatplutusscriptaddr"
echo "Spendable:           $spendable"
echo "Fee:                 $txfee"


cardano-cli transaction build-raw \
  --alonzo-era \
  --fee "$txfee" \
  --tx-in "$scriptownertxin" \
  --tx-in-collateral "$scriptownerCollateral" \
  --mint-script-file "$plutusscriptinuse" \
  --mint-redeemer-file "$redeemer" \
  --mint-execution-units "($plutusrequiredspace, $plutusrequiredtime)" \
  --tx-out "$dummyaddress+$spendable + 5 $policyid.MillarCoin" \
  --mint "5 $policyid.MillarCoin" \
  --protocol-params-file example/pparams.json \
  --out-file "$work/plutusmint.body"

cardano-cli transaction sign \
  --tx-body-file "$work/plutusmint.body" \
  --testnet-magic 42 \
  --signing-key-file "$targetskey" \
  --out-file "$work/plutusmint.tx"

# SUBMIT
cardano-cli transaction submit --tx-file "$work/plutusmint.tx" --testnet-magic 42

echo "Pausing for 5 seconds..."
sleep 5
cardano-cli query utxo --whole-utxo --testnet-magic 42