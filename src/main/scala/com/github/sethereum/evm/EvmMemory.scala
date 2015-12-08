package com.github.sethereum.evm

import java.math.BigInteger

import com.github.sethereum.evm.EvmWordConversions._

import math.Ordering.Implicits._
import scala.util.Try

trait EvmMemoryOps[T <: EvmMemoryOps[T]] { this: T =>
  def size: EvmWord
  def memLoad(begin: EvmWord): Try[(EvmWord, T)]
  def memStore(begin: EvmWord, value: EvmWord): Try[T]
  def memStore(begin: EvmWord, value: Byte): Try[T]
}

/**
 * Simple auto sizing memory implementation backed by a single contiguous byte array.
 */
case class EvmMemory private (memory: Map[EvmWord, EvmWord] = Map.empty.withDefault(_ => EvmWord.ZERO), end: BigInteger = BigInteger.ZERO) extends EvmMemoryOps[EvmMemory] {
  import EvmMemory._

  // Size in bytes
  override def size: EvmWord = end.add(EvmWordBytes).divide(EvmWordBytes).multiply(EvmWordBytes)

  override def memLoad(offset: EvmWord): Try[(EvmWord, EvmMemory)] = {
    val begin = (offset: BigInteger)
    val end = begin.add(EvmWordBytes)
    val rem = begin.mod(EvmWordBytes)
    val isAligned = rem == BigInteger.ZERO

    // Shortcut word aligned memory access
    if (isAligned) {
      Try((memory(begin.padLeft), if (end > this.end) copy(end = end) else this))
    } else {
      val aligned = begin.subtract(rem)
      val first = memory(aligned.padLeft)
      val second = memory(aligned.add(EvmWordBytes).padLeft)

      val len = rem.intValue()
      val bytes = Array.ofDim[Byte](EvmWord.BYTES)
      first.bytes.copyToArray(bytes, 0, len)
      second.bytes.copyToArray(bytes, len, EvmWord.BYTES - len)

      Try((EvmWord(bytes), if (end > this.end) copy(end = end) else this))
    }
  }

  override def memStore(begin: EvmWord, value: EvmWord): Try[EvmMemory] = ???

  override def memStore(begin: EvmWord, value: Byte): Try[EvmMemory] = ???
}

object EvmMemory {
  val EvmWordBytes: BigInteger = BigInteger.valueOf(EvmWord.BYTES)
}