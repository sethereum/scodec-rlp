package com.github.sethereum.evm

sealed trait EvmTransaction {
  val nonce: EvmNonce
  val gasPrice: P256
  val gasLimit: P256
//  val to: EvmAddress
  val value: P256
  val v: P5
  val r: B32
  val s: B32

  requireNonce(nonce)
  requireP256(gasPrice)
  requireP256(gasLimit)
  requireBalance(value)
  // Appendix F.206
  require(v >= 27 && v <= 28, s"Invalid ECDSASIGN recovery id $v")
  requireB32(r)
  requireB32(s)

  // TODO: Implement signature recovery function
  // See: https://forum.ethereum.org/discussion/2166/signature-generation-and-verification-in-solidity
  lazy val sender: EvmAddress = EvmAddress.ZERO
}

object EvmTransaction {

}


case class EvmCreateTransaction(
  nonce: EvmNonce,
  gasPrice: P256,
  gasLimit: P256,
  value: P256,
  v: P5, r: B32, s: B32,
  init: EvmProgram
) extends EvmTransaction

case class EvmCallTransaction(
  nonce: EvmNonce,
  gasPrice: P256,
  gasLimit: P256,
  to: EvmAddress,
  value: P256,
  v: P5, r: B32, s: B32,
  data: Seq[Byte]
) extends EvmTransaction
