package com.github.sethereum.evm

import java.math.BigInteger

import com.github.sethereum.evm.EvmWordConversions._

import scala.util.Try

trait EvmMemoryOps[T <: EvmMemoryOps[T]] { this: T =>
  def size: EvmWord
  def memLoad(begin: EvmWord): Try[(EvmWord, T)]
  def memStore(begin: EvmWord, value: EvmWord): Try[T]
  def memStore(begin: EvmWord, value: Byte): Try[T]
}

case class EvmMemory private (values: Map[EvmWord, EvmWord] = Map.empty.withDefault(_ => EvmWord.ZERO), end: EvmWord = EvmWord.ZERO) extends EvmMemoryOps[EvmMemory] {
  import EvmMemory._
  
  override def size: EvmWord = (end: BigInteger).multiply(EvmWordSize)

  override def memLoad(offset: EvmWord): Try[(EvmWord, EvmMemory)] = {
    val begin = (begin: BigInteger)
    val rem = begin.mod(EvmWordSize).intValue()
    val max = end.max(begin.add(EvmWordSize).add(BigInteger.ONE).divide(EvmWordSize))

    if (offset == 0) {
      Try((values(begin), ))
    }
  }

  override def memStore(begin: EvmWord, value: EvmWord): Try[EvmMemory] = ???

  override def memStore(begin: EvmWord, value: Byte): Try[EvmMemory] = ???
}

object EvmMemory {
  val EvmWordSize: BigInteger = BigInteger.valueOf(EvmWord.BYTES)
}