package com.github.sethereum

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

  type EvmValue = BigInt

}
