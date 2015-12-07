package com.github.sethereum.evm

import java.util

import scala.language.implicitConversions

final class EvmAddress(val bytes: Array[Byte]) {

  require(bytes.length <= EvmWord.BYTES, s"invalid address length ${bytes.length}")

  def apply = bytes

  override def equals(other: Any): Boolean = other match {
    case that: EvmWord => util.Arrays.equals(bytes, that.bytes)
    case _ => false
  }

  override def hashCode(): Int = util.Arrays.hashCode(bytes)
}

object EvmAddress {
  val SIZE = 160
  val BYTES = SIZE / 8
  val ZERO = EvmAddress(Array.fill(BYTES)(0.toByte))

  def apply(bytes: Array[Byte]): EvmAddress = new EvmAddress(bytes)

  // Implicit conversion to byte array
  implicit def addressToBytes(address: EvmAddress): Array[Byte] = address.apply
  implicit def bytesToAddress(bytes: Array[Byte]): EvmAddress = EvmAddress(bytes)
}
