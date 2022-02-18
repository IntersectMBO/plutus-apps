"use strict";

const buildTx = require('nami-wallet/src/api/extension/wallet').buildTx;
const Loader = require('nami-wallet/src/api/loader').default;

exports.enableImpl = () => window.cardano.enable()

exports.isEnabled = window.cardano.isEnabled

// TODO This should belong in the API of Nami wallet
exports.getWalletIdImpl = () => {
  return Promise.all([window.cardano.getUsedAddresses(), window.cardano.getUnusedAddresses()])
    .then(([walletUsedAddresses, walletUnusedAddresses]) => {
      const addresses = walletUnusedAddresses.concat(walletUsedAddresses)
      return require('blake2b')(20)
        .update(Buffer.from(addresses.map(a => a.to_bech32).join('')))
        .digest('hex')
    })
}

exports.getNetworkIdImpl = window.cardano.getNetworkId

exports.getBalanceImpl = window.cardano.getBalance

exports.getUtxosImpl = amount => pagination => () => {
  return window.cardano.getUtxos(amount, pagination)
}

exports.getCollateralImpl = window.cardano.getCollateral

exports.getUsedAddressesImpl = window.cardano.getUsedAddresses

exports.getUnusedAddressesImpl = window.cardano.getUnusedAddresses

exports.getChangeAddressImpl = window.cardano.getChangeAddress

exports.getRewardAddressImpl = window.cardano.getRewardAddress

exports.signDataImpl = address => payload => () => window.cardano.signData(address, payload)

// TODO Function 'balanceTx' needs to be available in the Nami wallet API.
exports.balanceTxImpl = txCbor => () => {
  return Loader.load()
    .then(() => {
      const CardanoWasm = Loader.Cardano
      return Promise.all([
        window.cardano.getChangeAddress(),
        window.cardano.getUtxos(),
        fetchProtocolParameters()
      ])
      .then(promises => {
        const changeAddrCbor = promises[0]
        const changeAddrBech32 = CardanoWasm.Address.from_bytes(fromHexString(changeAddrCbor)).to_bech32()
        const utxosCbor = promises[1]
        const utxos = utxosCbor.map(cbor => CardanoWasm.TransactionUnspentOutput.from_bytes(fromHexString(cbor)))
        const pp = promises[2]
        const tx = CardanoWasm.Transaction.from_bytes(fromHexString(txCbor))
        return buildTx({ 'paymentAddr': changeAddrBech32 }, utxos, tx.body().outputs(), pp)
      })
      .then(btx => toHexString(btx.to_bytes()))
    })
}

exports.signTxImpl = tx => partialSign => () => window.cardano.signTx(tx, partialSign)

exports.submitTxImpl = tx => () => window.cardano.submitTx(tx)

// exports.onAccountChangeImpl = window.cardano.onAccountChange
// exports.onNetworkChangeImpl = window.cardano.onNetworkChange

// TODO Unecessary when 'balanceTx' is merged in Nami wallet API
const fetchProtocolParameters = () => {
  return fetch('https://cardano-testnet.blockfrost.io/api/v0/blocks/latest', {
      headers: {
        'project_id': 'testnetav02u9IsCvAzz50tCzKIhDpmtxEWOpuv'
      },
    })
    .then(res => res.json())
    .then(latestBlock => {
      return fetch(`https://cardano-testnet.blockfrost.io/api/v0/epochs/${latestBlock.epoch}/parameters`, {
          headers: {
            'project_id': 'testnetav02u9IsCvAzz50tCzKIhDpmtxEWOpuv'
          },
        })
        .then(res => res.json())
        .then(p => {
          return {
            linearFee: {
              minFeeA: p.min_fee_a.toString(),
              minFeeB: p.min_fee_b.toString(),
            },
            minUtxo: '1000000', //p.min_utxo, minUTxOValue protocol paramter has been removed since Alonzo HF. Calulation of minADA works differently now, but 1 minADA still sufficient for now
            poolDeposit: p.pool_deposit,
            keyDeposit: p.key_deposit,
            coinsPerUtxoWord: '34482',
            maxValSize: 5000,
            priceMem: 5.77e-2,
            priceStep: 7.21e-5,
            maxTxSize: parseInt(p.max_tx_size),
            slot: parseInt(latestBlock.slot),
          };
        })
    })
}

// TODO Unecessary when 'balanceTx' is merged in Nami wallet API
const fromHexString = hexString =>
  new Uint8Array(hexString.match(/.{1,2}/g).map(byte => parseInt(byte, 16)));

// TODO Unecessary when 'balanceTx' is merged in Nami wallet API
// padd with leading 0 if <16
const i2hex = i => ('0' + i.toString(16)).slice(-2)

// TODO Unecessary when 'balanceTx' is merged in Nami wallet API
const toHexString = uint8 => Array.from(uint8).map(i2hex).join('');
