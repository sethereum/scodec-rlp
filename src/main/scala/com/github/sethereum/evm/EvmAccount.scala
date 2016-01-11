package com.github.sethereum.evm

import shapeless.HNil

sealed trait EvmAccount {
  val nonce       : EvmNumber
  val balance     : EvmBalance
  val storageRoot : EvmHash
  val codeHash    : EvmHash
}

object EvmAccount {
  def apply(nonce: EvmNumber, balance: EvmBalance, storageRoot: EvmHash, codeHash: EvmHash): EvmAccount = codeHash match {
    case EvmHash.Empty => EvmSimpleAccount(nonce, balance, storageRoot)
    case _ => EvmContractAccount(nonce, balance, storageRoot, codeHash)
  }
  def unapply(hlist : (EvmNumber :: EvmBalance :: EvmHash :: EvmHash)): Option[EvmAccount] = hlist match {
    case nonce :: balance :: storageRoot :: EvmHash.Empty => EvmSimpleAccount(nonce, balance, storageRoot)
  }
}

case class EvmSimpleAccount private[evm] (
  nonce       : EvmNumber,
  balance     : EvmBalance,
  storageRoot : EvmHash
) extends EvmAccount {
  val codeHash = EvmHash.Empty
}

case class EvmContractAccount private[evm] (
  nonce       : EvmNumber,
  balance     : EvmBalance,
  storageRoot : EvmHash,
  codeHash    : EvmHash
) extends EvmAccount
