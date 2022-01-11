"use strict"

exports.fromBytesImpl = cardanoWasm => bytes => onError => onSuccess => {
  try {
    return onSuccess(cardanoWasm.Transaction.from_bytes(bytes));
  } catch (e) {
    return onError(e);
  }
}

exports.toBytes = transaction => transaction.to_bytes()

exports.newImpl = cardanoWasm => body => witnessSet => onError => onSuccess => {
  try {
    // TODO Put AuxiliaryData instead of undefined
    return onSuccess(cardanoWasm.Transaction.new(body, witnessSet, undefined))
  } catch (e) {
    return onError(e);
  }
}

exports.body = transaction => transaction.body()
