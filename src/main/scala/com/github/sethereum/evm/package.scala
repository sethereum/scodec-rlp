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

    object Key {
      def apply(bytes: Array[Byte]): EvmStorage.Key = EvmWord(bytes)
    }
    object Value {
      def apply(bytes: Array[Byte]): EvmStorage.Value = EvmWord(bytes)
    }
  }

  type EvmValue = BigInteger

  /**
   * Converstions to/from EVM word.
   */
  object EvmWordConversions {

    // Int
    // TODO: Properly handle signed ints
    implicit def wordToInt(word: EvmWord) = new BigInteger(word.bytes).intValue()
    implicit def intToWord(value: Int): EvmWord = EvmWord(BigInteger.valueOf(value).toByteArray)


    // BigInteger
    implicit def wordToBigInteger(word: EvmWord) = new BigInteger(word.bytes)
    implicit def bigIntegerToWord(value: BigInteger): EvmWord = EvmWord(value.toByteArray)

    // Address
    implicit def wordToAddress(word: EvmWord) = new EvmAddress(word.bytes)
    implicit def addressToWord(value: EvmAddress): EvmWord = EvmWord(value.data)

  }


}
