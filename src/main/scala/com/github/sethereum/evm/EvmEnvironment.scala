package com.github.sethereum.evm

import java.math.BigInteger

case class EvmEnvironment(
  address: EvmAddress = EvmAddress.Zero,
  origin: EvmAddress = EvmAddress.Zero,
  call: EvmCall = EvmCall(),
  block: EvmBlock = EvmBlock()
)

case class EvmCall(
  caller: EvmAddress = EvmAddress.Zero,
  callValue: EvmValue = BigInteger.ZERO,
  callData: Seq[Byte] = Seq.empty
)