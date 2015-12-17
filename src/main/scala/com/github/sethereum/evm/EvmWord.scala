package com.github.sethereum.evm

import java.math.BigInteger

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


  object ImplicitConversions {

    // Byte array
    implicit def wordToByteArray(word: EvmWord): Array[Byte] = word.data.toArray[Byte]
    implicit def byteArrayToWord(array: Array[Byte]): EvmWord = EvmWord(array)
    
    // Byte sequence
    implicit def wordToByteSeq(word: EvmWord): Seq[Byte] = word.data
    implicit def byteSeqToWord(bytes: Seq[Byte]): EvmWord = EvmWord(bytes)

    // Int
    // TODO: Properly handle signed ints
    implicit def wordToInt(word: EvmWord) = new BigInteger(word.bytes).intValue()
    implicit def intToWord(value: Int): EvmWord = EvmWord(BigInteger.valueOf(value).toByteArray)


    // BigInt
    implicit def wordToBigInt(word: EvmWord) = BigInt(word.bytes)
    implicit def bigIntToWord(value: BigInt): EvmWord = EvmWord(value.toByteArray)

    // Address
    implicit def wordToAddress(word: EvmWord) = new EvmAddress(word.bytes)
    implicit def addressToWord(value: EvmAddress): EvmWord = EvmWord(value.data)

  }
}
