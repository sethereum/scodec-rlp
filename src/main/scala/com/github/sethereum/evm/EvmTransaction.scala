package com.github.sethereum.evm

sealed trait EvmTransaction {
  val nonce: Long
  val gasPrice: Long
  val gasLimit: Long
  val to: EvmAddress
  val value: Long
}

case class EvmCallTransaction(nonce: Long, gasPrice: Long, gasLimit: Long, to: EvmAddress, value: Long) extends EvmTransaction
