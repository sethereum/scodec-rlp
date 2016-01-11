package com.github.sethereum.evm

import shapeless._

import scala.util.Try

sealed trait EvmTransaction {
  val nonce     : EvmNumber
  val gasPrice  : EvmPrice
  val gasLimit  : EvmGas
  val to        : Option[EvmAddress]
  val value     : EvmValue
  val v         : EvmRecoveryId
  val r         : B32
  val s         : B32

  def payload: B

  def hlist: EvmTransaction.HList =nonce :: gasPrice :: gasLimit :: to :: value :: payload :: v :: r :: s :: HNil

  // TODO: Implement signature recovery function
  // See: https://forum.ethereum.org/discussion/2166/signature-generation-and-verification-in-solidity
  lazy val sender: EvmAddress = EvmAddress.Zero
}

object EvmTransaction {

  type HList = EvmNumber :: EvmPrice :: EvmGas :: Option[EvmAddress] :: EvmValue :: B :: EvmRecoveryId :: B32 :: B32 :: HNil

  def apply(
    nonce     : EvmNumber,
    gasPrice  : EvmPrice,
    gasLimit  : EvmGas,
    to        : Option[EvmAddress],
    value     : EvmValue,
    payload   : B,
    v         : EvmRecoveryId,
    r         : B32,
    s         : B32
  ): Try[EvmTransaction] = to match {
    case t @ Some(_) => Try(EvmCallTransaction(nonce, gasPrice, gasLimit, t, value, payload, v, r, s))
    case None => EvmProgram(payload).map(p => EvmCreateTransaction(nonce, gasPrice, gasLimit, value, p, v, r, s))
  }

  def apply(hlist: EvmTransaction.HList): Try[EvmTransaction] = hlist match {
    case nonce :: gasPrice :: gasLimit :: to :: value :: payload :: v :: r :: s :: HNil =>
      EvmTransaction(nonce, gasPrice, gasLimit, to, value, payload, v, r, s)
  }
}

case class EvmCreateTransaction(
  nonce     : EvmNumber,
  gasPrice  : EvmPrice,
  gasLimit  : EvmGas,
  value     : EvmValue,
  init      : EvmProgram,
  v         : EvmRecoveryId,
  r         : B32,
  s         : B32
) extends EvmTransaction {
  override val to = Option.empty

  override def payload: B = init.code
}

case class EvmCallTransaction(
  nonce     : EvmNumber,
  gasPrice  : EvmPrice,
  gasLimit  : EvmGas,
  to        : Some[EvmAddress],
  value     : EvmValue,
  data      : B,
  v         : EvmRecoveryId,
  r         : B32,
  s         : B32
) extends EvmTransaction {

  def this(
    nonce     : EvmNumber,
    gasPrice  : EvmPrice,
    gasLimit  : EvmGas,
    to        : EvmAddress,
    value     : EvmValue,
    data      : B,
    v         : EvmRecoveryId,
    r         : B32,
    s         : B32
  ) = this(nonce, gasPrice, gasLimit, Some(to), value, data, v, r, s)

  override def payload: B = data
}
