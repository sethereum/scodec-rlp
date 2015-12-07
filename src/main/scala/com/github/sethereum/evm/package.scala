package com.github.sethereum

import java.math.BigInteger

import scala.language.implicitConversions

package object evm {

  class EvmException(msg: String) extends Exception(msg) {
    def this() = this("")
  }

  type EvmWord = Array[Byte]
  object EvmWord {
    val SIZE = 256
    val BYTES = SIZE / 8
    val ZERO = Array.fill(32)(0.toByte)
  }

  type EvmStorage = Map[EvmStorage.Key, EvmStorage.Value]
  object EvmStorage {
    type Key = EvmWord
    type Value = EvmWord

    val empty = Map.empty[Key, Value].withDefaultValue(EvmWord.ZERO)
  }

  /**
   * Converstions to/from EVM word.
   */
  object EvmWordConversions {

    // BigInteger
    implicit def wordToBigInteger(word: EvmWord) = new BigInteger(word)
    implicit def bigIntegerToWord(value: BigInteger): EvmWord = value.toByteArray

  }


}
