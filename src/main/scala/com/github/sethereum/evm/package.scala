package com.github.sethereum

import scala.language.implicitConversions

package object evm {

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

}
