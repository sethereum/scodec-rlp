package com.github.sethereum.evm

import scala.language.implicitConversions

trait EvmWord2 {
  val data: Seq[Byte]
  def bytes: Array[Byte]
}


final case class EvmWord (val data: Seq[Byte]) {
  require(data.length <= EvmWord.BYTES, s"invalid word length ${data.length}")

  def bytes: Array[Byte] = data.toArray[Byte]

  def padLeft: EvmWord =
    if (data.length == EvmWord.BYTES) this
    else {
      val padded = Array.ofDim[Byte](EvmWord.BYTES)
      data.toArray[Byte].copyToArray(padded, EvmWord.BYTES - data.length)
      EvmWord(padded)
    }
}

object EvmWord {
  val SIZE = 256
  val BYTES = SIZE / 8
  val ZERO = EvmWord(Seq.fill(BYTES)(0.toByte))

  def apply(byte: Byte): EvmWord = new EvmWord(Seq(byte))

  def leftAlign(bytes: Seq[Byte]): EvmWord = {
    require(bytes.length <= BYTES, s"invalid bytes length ${bytes.length}")
    val padded = Array.ofDim[Byte](BYTES)
    bytes.copyToArray(padded)
    EvmWord(padded)
  }

  // Implicit conversion to byte array
  implicit def wordToBytes(word: EvmWord): Array[Byte] = word.data.toArray[Byte]
  implicit def bytesToWord(array: Array[Byte]): EvmWord = EvmWord(array)
}
