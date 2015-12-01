package com.github.sethereum

import java.math.BigInteger

import scala.language.implicitConversions

package object evm {

  type EvmWord = Array[Byte]
  object EvmWord {
    val SIZE = 256
    val BYTES = SIZE / 8
  }


  // EVM word conversions

  implicit def wordToBigInteger(word: EvmWord) = new BigInteger(word)
  implicit def bigIntegerToWord(value: BigInteger): EvmWord = value.toByteArray



}
