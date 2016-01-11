package com.github.sethereum.evm

case class EvmEnvironment(
  address: EvmAddress = EvmAddress.Zero,
  origin: EvmAddress = EvmAddress.Zero,
  call: EvmCall = EvmCall(),
  block: EvmBlock = EvmBlock()
)

case class EvmCall(
  caller: EvmAddress = EvmAddress.Zero,
  callValue: EvmValue = EvmValue.Zero,
  callData: Seq[Byte] = Seq.empty
)