package com.github.sethereum.evm

sealed trait EvmTransaction {
  val nonce     : EvmNumber
  val gasPrice  : EvmPrice
  val gasLimit  : EvmGas
  val to        : Option[EvmAddress]
  val value     : EvmValue
  val v         : EvmRecoveryId
  val r         : B32
  val s         : B32

  // TODO: Implement signature recovery function
  // See: https://forum.ethereum.org/discussion/2166/signature-generation-and-verification-in-solidity
  lazy val sender: EvmAddress = EvmAddress.Zero
}

object EvmTransaction {

}


case class EvmCreateTransaction(
  nonce     : EvmNumber,
  gasPrice  : EvmPrice,
  gasLimit  : EvmGas,
  value     : EvmValue,
  v         : EvmRecoveryId,
  r         : B32,
  s         : B32,
  init      : EvmProgram
) extends EvmTransaction {
  override val to = Option.empty
}

case class EvmCallTransaction(
  nonce     : EvmNumber,
  gasPrice  : EvmPrice,
  gasLimit  : EvmGas,
  to        : Some[EvmAddress],
  value     : EvmValue,
  v         : EvmRecoveryId,
  r         : B32,
  s         : B32,
  data      : B
) extends EvmTransaction
