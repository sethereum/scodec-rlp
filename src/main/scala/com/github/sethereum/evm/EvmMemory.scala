package com.github.sethereum.evm

import com.github.sethereum.evm.EvmWord.ImplicitConversions._

import scala.util.Try

trait EvmMemoryOps[T <: EvmMemoryOps[T]] { this: T =>
  def memSize: EvmWord
  def memLoad(offset: Int): Try[(EvmWord, T)]
  def memSlice(begin: Int, end: Int): Try[(Seq[Byte], T)]
  def memStore(offset: Int, value: EvmWord): Try[T]
  def memStore(offset: Int, value: Byte): Try[T]
  def memStore(offset: Int, value: Seq[Byte]): Try[T]
}

/**
 * Simple auto sizing memory implementation backed by a single contiguous byte array.
 *
 * Implementation via byte array has the following consequences:
 * 1. Does NOT satisfy immutable state semantics given the underlying array
 * 2. It is not optimized for sparse memory access
 * 3. Addressable memory is limited to the size of an Int
 */
case class EvmMemory private (bytes: Seq[Byte] = Seq.empty, max: Int = 0) extends EvmMemoryOps[EvmMemory] {

  // Round size up to 32 byte boundary
  def memSize(size: Int): Int = (size + EvmWord.BYTES - 1) / EvmWord.BYTES * EvmWord.BYTES

  override def memSize: EvmWord = memSize(max)

  override def memLoad(offset: Int): Try[(EvmWord, EvmMemory)] = {
    val begin = offset: Int
    val end = begin + EvmWord.BYTES

    if (begin >= bytes.size) {
      Try((EvmWord.ZERO, copy(max = Math.max(this.max, end))))
    } else if (end <= bytes.size) {
      Try((EvmWord(bytes.slice(begin, end)), this))
    } else {
      Try((EvmWord.leftAlign(bytes.slice(begin, bytes.size)), copy(max = end)))
    }
  }

  override def memSlice(begin: Int, end: Int): Try[(Seq[Byte], EvmMemory)] = {
    if (begin >= bytes.size) {
      Try((Seq.fill(end - begin)(0.toByte), copy(max = end)))
    } else if (end <= bytes.size) {
      Try((bytes.slice(begin, end), this))
    } else {
      Try((bytes.slice(begin, end).padTo(end - begin, 0.toByte), copy(max = end)))
    }
  }

  override def memStore(offset: Int, value: EvmWord): Try[EvmMemory] = {
    val end = offset + EvmWord.BYTES
    val to: Array[Byte] = atLeastMem(end)

    value.padLeft.bytes.copyToArray(to, offset)
    Try(EvmMemory(to, Math.max(this.max, end)))
  }

  override def memStore(offset: Int, value: Byte): Try[EvmMemory] = {
    val end = offset + 1
    val to: Array[Byte] = atLeastMem(end)

    to(offset) = value
    Try(EvmMemory(to, Math.max(this.max, end)))
  }

  override def memStore(offset: Int, value: Seq[Byte]): Try[EvmMemory] = {
    val end = offset + value.size
    val to: Array[Byte] = atLeastMem(end)

    value.copyToArray(to, offset)
    Try(EvmMemory(to, Math.max(this.max, end)))
  }

  // Ensure that internal memory array is at least the given size
  // Allocation size, if needed, is rounded up to 32 byte boundary
  private def atLeastMem(size: Int): Array[Byte] = {
    if (size > bytes.size) {
      val bytes = Array.ofDim[Byte](memSize(size))
      this.bytes.copyToArray(bytes)
      bytes
    } else {
      this.bytes.toArray
    }
  }
}

object EvmMemory {
  def apply(): EvmMemory = new EvmMemory()

  def apply(bytes: Seq[Byte]): EvmMemory = new EvmMemory(bytes, bytes.size)
}

//// Sparse Map-based memory implementation
//case class EvmMemory private (memory: Map[EvmWord, EvmWord] = Map.empty.withDefault(_ => EvmWord.ZERO), end: BigInteger = BigInteger.ZERO) extends EvmMemoryOps[EvmMemory] {
//  import EvmMemory._
//
//  // Size in bytes
//  override def size: EvmWord = end.add(EvmWordBytes).divide(EvmWordBytes).multiply(EvmWordBytes)
//
//  override def memLoad(offset: EvmWord): Try[(EvmWord, EvmMemory)] = {
//    val begin = (offset: BigInteger)
//    val end = begin.add(EvmWordBytes)
//    val rem = begin.mod(EvmWordBytes)
//    val isAligned = rem == BigInteger.ZERO
//
//    // Shortcut word aligned memory access
//    if (isAligned) {
//      Try((memory(begin.padLeft), if (end > this.end) copy(end = end) else this))
//    } else {
//      val aligned = begin.subtract(rem)
//      val first = memory(aligned.padLeft)
//      val second = memory(aligned.add(EvmWordBytes).padLeft)
//
//      val len = rem.intValue()
//      val bytes = Array.ofDim[Byte](EvmWord.BYTES)
//      first.bytes.copyToArray(bytes, 0, len)
//      second.bytes.copyToArray(bytes, len, EvmWord.BYTES - len)
//
//      Try((EvmWord(bytes), if (end > this.end) copy(end = end) else this))
//    }
//  }
//
//  override def memStore(begin: EvmWord, value: EvmWord): Try[EvmMemory] = ???
//
//  override def memStore(begin: EvmWord, value: Byte): Try[EvmMemory] = ???
//}
//
//object EvmMemory {
//  val EvmWordBytes: BigInteger = BigInteger.valueOf(EvmWord.BYTES)
//}