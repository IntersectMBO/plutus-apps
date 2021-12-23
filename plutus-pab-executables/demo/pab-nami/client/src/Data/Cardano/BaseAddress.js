"use strict"

exports.fromAddressImpl = cardanoWasm => address => {
  return cardanoWasm.BaseAddress.from_address(address)
}

exports.paymentCred = baseAddress => {
  return baseAddress.payment_cred()
}

exports.stakeCred = baseAddress => {
  return baseAddress.stake_cred()
}
