package com.github.sethereum.evm

import com.github.sethereum.evm.codecs._
import com.github.sethereum.rlp._
import scodec.Attempt.{Failure, Successful}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._
import scodec.{Attempt, Codec, Err}

import collection.mutable
import scala.concurrent.Future
import scala.util.{Success, Try}


case class EvmTrie(root: EvmHash)(implicit db: EvmTrieDb) {

  def get(key: B): Future[Option[B]] = ???
  def put(key: B, value: B): Future[EvmTrie] = ???
  def remove(key: B): Future[(EvmTrie, Option[B])] = ???

}

trait EvmTrieDb {
  def get(hash: EvmHash): Future[Option[EvmTrie.Node]]
  def put(node: EvmTrie.Node): Future[EvmHash]
}


object EvmTrie {

  case class Key(leaf: Boolean, nibbles: List[Nibble])

  sealed abstract class Node
  sealed abstract class KeyedNode extends Node { val key: List[Nibble] }

  case object EmptyNode extends Node

  case class LeafNode(key: List[Nibble], value: B) extends KeyedNode

  case class ExtensionNode(key: List[Nibble], value: EvmHash) extends KeyedNode {
    require(key.size > 1, s"invalid extension node key size (size: ${key.size})")
  }
  case class BranchNode(nodes: List[Node], value: Option[B]) extends Node

  case class ExtensionNode1(key: List[Nibble], value: Either[EvmHash, Node]) extends KeyedNode {
    require(key.size > 1, s"invalid extension node key size (size: ${key.size})")
    // Note: value can not be an extension node
  }
  case class BranchNode1(nodes: List[Either[EvmHash, Node]], value: Option[B]) extends Node


  sealed trait CappedNode { val key: B_32 }
  case class DirectNode private[evm] (key: B_32) extends CappedNode
  case class IndirectNode private[evm] (key: EvmHash, value: B) extends CappedNode
  object CappedNode {
    def apply(node: Node): CappedNode = {
      val encoded = rnode.encode(node).map(_.toByteArray.toSeq).require
      if (encoded.size < 32) DirectNode(encoded)
      else IndirectNode(EvmHash.keccak256(encoded), encoded)
    }
  }



  val rnode: RlpCodec[Node] = rlpCodec(recover(rbyteseq0).consume[Node] { empty =>
    if (empty) provide(EmptyNode)
    else nonEmptyNode
  } { _ == EmptyNode })

  val nodecap: Codec[Either[EvmHash, Node]] = bytes.narrow[Either[EvmHash, Node]](bytes =>
    bytes.size match {
      case 0 => Successful(Right(EmptyNode))
      case size if size < 32 => nonEmptyNode.decode(bytes.bits).map(r => Right(r.value))
      case size if size == 32 => Successful(Left(EvmHash(bytes.toSeq)))
      case size => Failure(Err(s"unexpected capped node size (size: $size, bytes: $bytes)"))
    }, _ match {
      case Left(hash) => ByteVector(hash)
      case Right(node) => rnode.encode(node).require.bytes
    })

  val nonEmptyNode = rlpCodec(rlist(rbytes).narrow[Node](
    list => list.size match {
      case 2 => key.decode(list(0).bits).map(_.value match {
        case Key(true, k) => LeafNode(k, list(1).toSeq)
        case Key(false, k) => ExtensionNode(k, EvmHash(list(1).toSeq))
      })
      case 17 => list.splitAt(16) match { case (branches, value :: Nil) =>
        branches.foldLeft(Attempt.successful(List[Node]())) { case (attempt, bytes) =>
          attempt.flatMap(nodes => rnode.decode(bytes.bits).map(r => nodes :+ r.value))
        }.flatMap { nodes =>
          rbyteseqOpt.decode(value.bits).map(r => BranchNode(nodes, r.value))
        }
      }
      case len => Failure(Err(s"invalid node list size $len"))
    },
    _ match {
      case LeafNode(k, v) => List(key.encode(Key(true, k)).require.bytes, ByteVector(v))
      case ExtensionNode(k, v) => List(key.encode(Key(false, k)).require.bytes, ByteVector(v))
      case BranchNode(ns, v) => ns.map(n => rnode.encode(n).require.bytes) ++ List(v.map(ByteVector.apply).getOrElse(ByteVector.empty))
    }
  ))

  // Key = prefix + optional padding nibble + key nibbles
  val key: Codec[Key] = constant(BitVector(0, 0)) ~> (
    ("leaf" | bool(1)) ::
    ("odd" | bool(1)).consume(odd => (if (odd) ignore(0) else ignore(4)) ~> nibbles)(_.size % 2 == 1)
  ).as[Key]

}




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