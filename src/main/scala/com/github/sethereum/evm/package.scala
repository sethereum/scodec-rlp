package com.github.sethereum

import scala.language.implicitConversions

package object evm {

  type EvmWord = Array[Byte]
  object EvmWord {
    val SIZE = 256
    val BYTES = SIZE / 8
  }


}
