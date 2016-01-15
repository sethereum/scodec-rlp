package com.github.sethereum.evm

import com.github.sethereum.evm.EvmTrie.NodeRepository
import com.github.sethereum.rlp._
import scodec.Attempt.{Failure, Successful}
import scodec.{Err, Codec}
import scodec.bits.{ByteVector, BitVector}

import scala.concurrent.Future
import scala.collection.mutable

import codecs._
import scodec.codecs._


//case class EvmTrie(repository: NodeRepository, rootHash: EvmHash) {
//
//  def get(key: B): Future[Option[B]]
//  def put(key: B, value: B): Future[EvmTrie]
//  def remove(key: B): Future[(EvmTrie, Option[B])]
//
//}

object EvmTrie {

//  sealed trait Key { val nibbles: List[Nibble] }
//  case class LeafKey(nibbles: List[Nibble]) extends Key
//  case class ExtensionKey(nibbles: List[Nibble]) extends Key

  case class Key(leaf: Boolean, nibbles: List[Nibble])

  type NodeHash = EvmHash

  sealed abstract class Node
  sealed abstract class KeyedNode extends Node { val key: List[Nibble] }

  case object EmptyNode extends Node
  case class LeafNode(key: List[Nibble], value: B) extends KeyedNode
  case class ExtensionNode(key: List[Nibble], value: NodeHash) extends KeyedNode {
    require(key.size > 1, s"invalid extension node key size (size: ${key.size})")
  }
  case class BranchNode(nodes: List[Node], value: Option[B]) extends Node


  object Node {

//    def apply(bytes: B): Node = bytes.size match {
//      case 0 => Some(Future.successful(EmptyNode))
//      case l if l < 32 =>
//      case 32 =>
//    }
  }

  trait NodeRepository {
    def get(hash: EvmHash): Future[Option[Node]]
    def put(hash: EvmHash, node: Node): Future[Unit]
  }

  val node = recover(rbyteseq0).consume[Node] { empty =>
    if (empty) provide(EmptyNode)
    else nonEmptyNode
  } { _ == EmptyNode }

//  val keyedNode = nibble.consume[KeyedNode](_ match { case HexPrefix(leaf, odd) =>
//    (if (odd) ignore(0) else ignore(4)) ~>
//      nibbles.consume[KeyedNode](key =>
//        if (leaf) LeafNode(LeafKey(n),
//        else ExtensionKey(n), _.nibbles)
//  })(HexPrefix.apply)

  val nonEmptyNode = rlist(rbytes).narrow[Node](
    list => list.size match {
      case 2 => for {
          key <- key.decode(list(0).bits)
          node <- key.value match {
            case Key(true, key) => LeafNode(key, list(1).toSeq)
            case Key(false, key) => ExtensionNode(key, EvmHash(list(1).toSeq))
          }
        } yield node
      case 17 => list.splitAt(16) match { case (branches, bytes) =>
        val nodes: List[Node] = for {
            branch <- branches
            node <- node.decode(branch.bits)
          } yield node.value
        for (value <- rbyteseqOpt.decode(bytes.head.bits))
          yield BranchNode(nodes, value.value)
      }
      case len => Failure(Err(s"invalid node list size $len"))
    },
    _ match {
      case LeafNode(k, v) => List(key.encode(Key(true, k)).require.bytes, rbyteseq.encode(v).require.bytes)
      case ExtensionNode(k, v) => List(key.encode(Key(false, k)).require.bytes, evmHash.encode(v).require.bytes)
      case BranchNode(ns, v) => ns.map(n => node.encode(n).require.bytes) ++ rbyteseqOpt.encode(v).require.bytes
    }
  )

  // Keyed node key hex prefix to indicate node type and nibbles parity
  // See Appendix C - Hex-Prefix Encoding
  case class HexPrefix(leaf: Boolean, odd: Boolean)
  object HexPrefix {
    def apply(key: Key): Nibble = key match {
      case LeafKey(n)      => Nibble(2 + (n.size % 2))
      case ExtensionKey(n) => Nibble(0 + (n.size % 2))
    }
    def apply(node: KeyedNode): Nibble = node match {
      case LeafNode(k, _)      => Nibble(2 + (k.size % 2))
      case ExtensionNode(k, _) => Nibble(0 + (k.size % 2))
    }
    // Extract (leaf, odd)
    def unapply(n: Nibble): Option[HexPrefix] =
      Some(HexPrefix((n & 2) != 0, (n & 1) != 0))
  }

  val hexPrefix2: Codec[HexPrefix] =
    constant(BitVector(0, 0)) ~>
      (("leaf" | bool(1)) ~ ("odd" | bool(1))).consume[HexPrefix] { case (leaf, odd) =>
        (if (odd) ignore(0) else ignore(4)) ~>
          provide(HexPrefix(leaf, odd))
    } { p => (p.leaf, p.odd) }

//  val hexPrefix: Codec[HexPrefix] =
//    constant(BitVector(0, 0)) ~>
//      (("leaf" | bool(1)).consume(leaf =>
//        ("odd" | bool(1)).consume(odd =>
//          (if (odd) ignore(0) else ignore(4)) ~>
//            provide(HexPrefix(leaf, odd)))))


//  val key2: Codec[List[Nibble]] = ("odd" | bool(1))
//    .consume(odd => (if (odd) ignore(0) else ignore(4)) ~> nibbles)(_.size % 2 == 1)

//  val keyedNode: Codec[KeyedNode] = key >>:~ { _ match {
//    case key @ Key(true, _) => LeafNode(key, )
//  }}

  // Key = prefix + optional padding nibble + key nibbles
  val key: Codec[Key] = constant(BitVector(0, 0)) ~> (
    ("leaf" | bool(1)) ::
    ("odd" | bool(1)).consume(odd => (if (odd) ignore(0) else ignore(4)) ~> nibbles)(_.size % 2 == 1)
  ).as[Key]

  // Key = prefix + optional padding nibble + key nibbles
//  val key: Codec[Key] = nibble.consume[Key](_ match { case HexPrefix(leaf, odd) =>
//    (if (odd) ignore(0) else ignore(4)) ~>
//      nibbles.xmap[Key](n => if (leaf) LeafKey(n) else ExtensionKey(n), _.nibbles)
//  })(HexPrefix.apply)

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