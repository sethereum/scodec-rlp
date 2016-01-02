package com.github.sethereum.evm

import java.math.BigInteger

case class EvmEnvironment(
  address: EvmAddress = EvmAddress.ZERO,
  origin: EvmAddress = EvmAddress.ZERO,
  call: EvmCall = EvmCall(),
  block: EvmBlock = EvmBlock()
)

case class EvmCall(
  caller: EvmAddress = EvmAddress.ZERO,
  callValue: EvmValue = BigInteger.ZERO,
  callData: Seq[Byte] = Seq.empty
)