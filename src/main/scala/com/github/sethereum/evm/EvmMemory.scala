package com.github.sethereum.evm

import scala.collection.immutable.SortedMap
import scala.util.Try

trait EvmMemoryOps[T <: EvmMemoryOps[T]] { this: T =>
  def memLoad(key: EvmWord): Try[EvmWord]
}

//case class EvmMemory(values: SortedMap[EvmWord, EvmWord] = SortedMap.empty, bytes: Int = 0)