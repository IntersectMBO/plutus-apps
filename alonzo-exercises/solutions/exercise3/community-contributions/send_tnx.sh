#!/usr/bin/bash
# usage:
# ./send_tnx.sh $SPEND_AMOUNT
# you need to have payment.addr and payment2.addr

# export testnet magic

export TESTNETMAGIC=1097911063

if [ -z "$1" ]
  then
    echo "please input amount you want to transfer"
    exit 1
fi

SPEND_AMOUNT=$1
echo "spending amount $SPEND_AMOUNT"

echo "getting utxos"
cardano-cli query utxo --address $(cat payment.addr) --testnet-magic $TESTNETMAGIC > utxos.txt

ADDR=`cat utxos.txt | tail -1 | awk '{print $1"#"$2}'`
echo "source address $ADD"

SRC_AMOUNT=`cat utxos.txt | tail -1 | awk '{print $3}'`
echo "source amount $SRC_AMOUNT"

cardano-cli transaction build-raw \
--tx-in $ADDR \
--tx-out $(cat payment2.addr)+0 \
--tx-out $(cat payment.addr)+0 \
--invalid-hereafter 0 \
--fee 0 \
--out-file tx.draft


# get protocol json
cardano-cli query protocol-parameters \
  --testnet-magic $TESTNETMAGIC \
  --out-file protocol.json

FEE=$(cardano-cli transaction calculate-min-fee \
    --tx-body-file tx.draft \
    --tx-in-count 1 \
    --tx-out-count 2 \
    --witness-count 1 \
    --byron-witness-count 0 \
    --testnet-magic $TESTNETMAGIC \
    --protocol-params-file protocol.json | grep -oP "\d+")
echo "tnx fee $FEE"    
 

REMAIN_AMOUNT=`expr $SRC_AMOUNT - $SPEND_AMOUNT - $FEE`
echo "remaining amoung $REMAIN_AMOUNT"

CURR_SLOT=`cardano-cli query tip --testnet-magic $TESTNETMAGIC | grep slot | grep -oP "\d+"` 
TARGET_SLOT=`expr $CURR_SLOT + 10`

echo "buiding raw tnx"
cardano-cli transaction build-raw \
--tx-in $ADDR \
--tx-out $(cat payment2.addr)+$SPEND_AMOUNT \
--tx-out $(cat payment.addr)+$REMAIN_AMOUNT \
--invalid-hereafter $TARGET_SLOT \
--fee $FEE \
--out-file tx.raw

echo "sign raw tnx"
cardano-cli transaction sign \
--tx-body-file tx.raw \
--signing-key-file payment.skey \
--testnet-magic $TESTNETMAGIC \
--out-file tx.signed

echo "submit tnx"
cardano-cli transaction submit \
--tx-file tx.signed \
--testnet-magic $TESTNETMAGIC

