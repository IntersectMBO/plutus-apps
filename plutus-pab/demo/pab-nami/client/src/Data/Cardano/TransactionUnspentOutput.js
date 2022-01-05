"use strict"

exports.fromBytesImpl = cardanoWasm => bytes => onError => onSuccess => {
  try {
    return onSuccess(cardanoWasm.TransactionUnspentOutput.from_bytes(bytes));
  } catch (e) {
    return onError(e);
  }
}
