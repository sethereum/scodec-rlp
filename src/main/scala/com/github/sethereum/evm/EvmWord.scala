package com.github.sethereum.evm

import java.util

import scala.language.implicitConversions

final class EvmWord(val bytes: Array[Byte]) {

  require(bytes.length <= EvmWord.SIZE, s"invalid word length ${bytes.length}")

  def apply = bytes

  override def equals(other: Any): Boolean = other match {
    case that: EvmWord => util.Arrays.equals(bytes, that.bytes)
    case _ => false
  }

  override def hashCode(): Int = util.Arrays.hashCode(bytes)
}

object EvmWord {
  val SIZE = 256
  val BYTES = SIZE / 8
  val ZERO = EvmWord(Array.fill(32)(0.toByte))

  def apply(word: Array[Byte]): EvmWord = new EvmWord(word)
  
  // Implicit conversion to byte array
  implicit def wordToBytes(word: EvmWord): Array[Byte] = word.apply
  implicit def bytesToWord(bytes: Array[Byte]): EvmWord = EvmWord(bytes)
}
