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
  def get(hash: EvmHash): Future[EvmTrie.Node]
  def put(node: EvmTrie.Node): Future[EvmHash]
}


object EvmTrie {

  type Nibbles = List[Nibble]
  type NodeEntry = Either[EvmHash, Node]

  case class Key(leaf: Boolean, nibbles: List[Nibble])

//  sealed trait NodeKey {
//    def apply(nibbles: Iterator[Nibble]): Option[]
//  }


  implicit class NodeEntryOps(val entry: NodeEntry) extends AnyVal {
    def node(implicit db: EvmTrieDb): Future[Node] = entry match {
      case Left(hash) => db.get(hash)
      case Right(node) => Future.successful(node)
    }
  }

  sealed abstract class Node
  sealed abstract class KeyedNode extends Node { val key: List[Nibble] }

  case object EmptyNode extends Node

  case class LeafNode(key: Nibbles, value: B) extends KeyedNode

  case class ExtensionNode(key: Nibbles, value: NodeEntry) extends KeyedNode {
    require(key.size > 1, s"invalid extension node key size (size: ${key.size})")
    // Note: value can not be an extension node
  }
  case class BranchNode(entries: List[NodeEntry], value: Option[B]) extends Node


  object codecs {

    val rnode: RlpCodec[Node] = rlpCodec(recover(rbyteseq0).consume[Node] { empty =>
      if (empty) provide(EmptyNode)
      else nonEmptyNode
    } { _ == EmptyNode })

    val nonEmptyNode = rlpCodec(rlist(rbytes).narrow[Node](
      list => list.size match {
        case 2 => key.decode(list(0).bits).map(_.value match {
          case Key(true, k) => LeafNode(k, list(1).toSeq)
          case Key(false, k) => ExtensionNode(k, Left(EvmHash(list(1).toSeq)))
        })
        case 17 => list.splitAt(16) match { case (branches, value) =>
          branches.foldLeft(Attempt.successful(List[NodeEntry]())) { case (attempt, branch) =>
            attempt.flatMap(entries => nodeEntry.decode(branch.bits).map(r => entries :+ r.value))
          }.flatMap { nodes =>
            rbyteseqOpt.decode(value.head.bits).map(r => BranchNode(nodes, r.value))
          }
        }
        case size => Failure(Err(s"invalid node list size $size"))
      },
      _ match {
        case LeafNode(k, v) => List(key.encode(Key(true, k)).require.bytes, ByteVector(v))
        case ExtensionNode(k, v) => List(key.encode(Key(false, k)).require.bytes, nodeEntry.encode(v).require.bytes)
        case BranchNode(es, v) => es.map(e => nodeEntry.encode(e).require.bytes) ++ List(v.map(ByteVector.apply).getOrElse(ByteVector.empty))
        case EmptyNode => List(ByteVector.empty)
      }
    ))

    val nodeEntry: Codec[Either[EvmHash, Node]] = bytes.narrow[Either[EvmHash, Node]](bytes =>
      bytes.size match {
        case 0 => Successful(Right(EmptyNode))
        case size if size < 32 => nonEmptyNode.decode(bytes.bits).map(r => Right(r.value))
        case size if size == 32 => Successful(Left(EvmHash(bytes.toSeq)))
        case size => Failure(Err(s"unexpected capped node size (size: $size, bytes: $bytes)"))
      }, _ match {
        case Left(hash) => ByteVector(hash.value)
        case Right(node) => rnode.encode(node).require.bytes
      })

    // Key = prefix + optional padding nibble + key nibbles
    val key: Codec[Key] = constant(BitVector(0, 0)) ~> (
      ("leaf" | bool(1)) ::
      ("odd" | bool(1)).consume(odd => (if (odd) ignore(0) else ignore(4)) ~> nibbles)(_.size % 2 == 1)
    ).as[Key]

  }

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