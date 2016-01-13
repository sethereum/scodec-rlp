package com.github.sethereum.evm

case class EvmEnvironment(
  address: EvmAddress = EvmAddress.Zero,
  originator: EvmTransaction = EvmCallTransaction(EvmNumber.Zero, EvmPrice.Zero, EvmGas.Zero, Some(EvmAddress.Zero), EvmValue.Zero, Seq.empty, EvmRecoveryId.Low, Seq.fill(32)(0.toByte), Seq.fill(32)(0.toByte)),
  call: EvmCall = EvmCall(),
  header: EvmBlock.Header = EvmBlock.Header(),
  block: EvmBlock = EvmBlock()
)

case class EvmCall(
  caller: EvmAddress = EvmAddress.Zero,
  callValue: EvmValue = EvmValue.Zero,
  callData: Seq[Byte] = Seq.empty
)