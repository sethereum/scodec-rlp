package com.github.sethereum.evm

import java.math.BigInteger

case class EvmEnvironment(
  address: EvmAddress = EvmAddress.ZERO,
  origin: EvmAddress = EvmAddress.ZERO,
  call: EvmCall
)

case class EvmCall(
  caller: EvmAddress = EvmAddress.ZERO,
  callValue: EvmValue = BigInteger.ZERO,
  callData: Array[Byte]
)