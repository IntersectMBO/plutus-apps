"use strict"

exports.fromBech32Impl = cardanoWasm => bechStr => onError => onSuccess => {
  try {
    return onSuccess(cardanoWasm.Address.from_bech32(bechStr));
  } catch (e) {
    return onError(e);
  }
}

exports.toBech32 = address => address.to_bech32();
