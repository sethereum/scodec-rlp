package com.github.sethereum.evm

import scala.language.implicitConversions

final class EvmAddress(val data: Seq[Byte]) {
  require(data.length == EvmAddress.BYTES, s"invalid address length ${data.length}")

  def this(array: Array[Byte]) = this(array: Seq[Byte])

  def bytes: Array[Byte] = data.toArray[Byte]
}

object EvmAddress {
  val SIZE = 160
  val BYTES = SIZE / 8
  val ZERO = EvmAddress(Array.fill(BYTES)(0.toByte))

  def apply(bytes: Array[Byte]): EvmAddress = new EvmAddress(bytes)

  // Implicit conversion to byte array
  implicit def addressToBytes(address: EvmAddress): Array[Byte] = address.data.toArray[Byte]
  implicit def bytesToAddress(array: Array[Byte]): EvmAddress = EvmAddress(array)
}
