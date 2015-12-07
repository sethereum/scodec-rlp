package com.github.sethereum

import java.math.BigInteger

import scala.language.implicitConversions

package object evm {

  class EvmException(msg: String) extends Exception(msg) {
    def this() = this("")
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
    implicit def wordToBigInteger(word: EvmWord) = new BigInteger(word.bytes)
    implicit def bigIntegerToWord(value: BigInteger): EvmWord = EvmWord(value.toByteArray)

  }


}
