package com.github.sethereum.evm

import scala.concurrent.Future

object EvmTrie {

  sealed trait Node

  case object EmptyNode extends Node
  case class NodeReference(hash: EvmHash) extends Node
  case class LeafNode(key: B, value: B_32) extends Node
  case class ExtensionNode(key: Byte, value: EvmHash) extends Node
  case class BranchNode(keys: Array[Byte], value: Option[B]) extends Node

  object Node {

    def unapply(bytes: B_32): Option[Future[Node]] = bytes.size match {
      case 0 => Some(Future.successful(EmptyNode))
//      case l if l < 32 =>
//      case 32 =>
    }
  }


  trait EvmNodeRepository {

    def node(hash: EvmHash): Future[Node]
  }

}




case class Nibble private[evm] (val value: Int) extends AnyVal

private[evm] class NibbleIterator(val bytes: Iterator[Byte], var bytesSize: Int) extends Iterator[Nibble] {
  private var byte: Int = _
  private var half = false

  override def size = bytesSize * 2

  override def hasDefiniteSize = true

  override def hasNext = half || bytes.hasNext

  override def next() = {
    if (half) {
      half = false
      bytesSize -= 1
      Nibble(byte & 0xF)
    } else {
      byte = bytes.next()
      half = true
      bytesSize -= 1
      Nibble((byte & 0xFF) >>> 4)
    }
  }
}

object NibbleOps {

  implicit class ByteArrayOps(val bytes: Array[Byte]) extends AnyVal {
    def nibbles: Iterator[Nibble] = new NibbleIterator(bytes.iterator, bytes.size)
  }

  implicit class StringOps(val str: String) extends AnyVal {
    def nibbles: Iterator[Nibble] = new NibbleIterator(str.iterator.map(_.toByte), str.size)
  }

  implicit class NibbleIteratorOps(val nibbles: Iterator[Nibble]) extends AnyVal {
    def bytes = new Iterator[Byte] {
      override def hasNext: Boolean = nibbles.hasNext
      // Note: Will intentionally fail if nibbles size is odd
      override def next(): Byte = ((nibbles.next().value << 4) & nibbles.next().value).toByte
    }
  }

}

// See Appendix C - Hex-Prefix Encoding
object HexPrefix {
  import NibbleOps._

  def apply(leaf: Boolean)(nibbles: Iterator[Nibble]): Array[Byte] = {
    val bytes = Array.ofDim[Byte]((nibbles.size + 2) / 2)
    val flag = if (leaf) 2 else 0

    if (nibbles.size % 2 == 0) {
      bytes(0) = (flag << 4).toByte
      nibbles.bytes.copyToArray(bytes, 1)
    } else {
      bytes(0) = (((flag + 1) << 4) & (nibbles.next().value)).toByte
      nibbles.bytes.copyToArray(bytes, 1)
    }

    bytes
  }

  def unapply(bytes: Array[Byte]): Option[(Boolean, Iterator[Nibble])] = {
    val flag = (bytes(0) & (2 << 4)) != 0
    val even = (bytes(0) & (1 << 4)) == 0

    if (even) {
      Some((flag, bytes.tail.nibbles))
    } else {
      val nibbles = bytes.nibbles
      nibbles.next()    // Consume header nibble
      Some((flag, nibbles))
    }
  }
}