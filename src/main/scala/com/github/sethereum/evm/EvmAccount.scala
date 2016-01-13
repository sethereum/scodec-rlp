package com.github.sethereum.evm

import shapeless.HNil
import shapeless._

sealed trait EvmAccount {
  val nonce       : EvmNumber
  val balance     : EvmBalance
  val storageRoot : EvmHash
  val codeHash    : EvmHash

  def hlist = nonce :: balance :: storageRoot :: codeHash :: HNil
}

object EvmAccount {

  type HList = EvmNumber :: EvmBalance :: EvmHash :: EvmHash :: HNil

  def apply(nonce: EvmNumber, balance: EvmBalance, storageRoot: EvmHash, codeHash: EvmHash): EvmAccount = codeHash match {
    case EvmHash.Empty => EvmSimpleAccount(nonce, balance, storageRoot)
    case _ => EvmContractAccount(nonce, balance, storageRoot, codeHash)
  }

  def apply(hlist: EvmAccount.HList): EvmAccount = hlist match {
    case nonce :: balance :: storageRoot :: codeHash :: HNil => EvmAccount(nonce, balance, storageRoot, codeHash)
  }
}

case class EvmSimpleAccount private[evm] (
  nonce       : EvmNumber = EvmNumber.Zero,
  balance     : EvmBalance,
  storageRoot : EvmHash = EvmHash.Empty
) extends EvmAccount {
  val codeHash = EvmHash.Empty
}

object EvmSimpleAccount {
  def apply(balance: EvmBalance): EvmSimpleAccount = new EvmSimpleAccount(balance = balance)
}

case class EvmContractAccount private[evm] (
  nonce       : EvmNumber = EvmNumber.Zero,
  balance     : EvmBalance,
  storageRoot : EvmHash = EvmHash.Empty,
  codeHash    : EvmHash
) extends EvmAccount

