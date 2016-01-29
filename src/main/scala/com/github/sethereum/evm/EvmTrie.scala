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

  object NodeEntry {

    val empty = Right(EmptyNode)

    def apply(node: Node)(implicit db: EvmTrieDb): Future[NodeEntry] = {
      Future(codecs.rnode.encode(node).require).flatMap(bits =>
        if (bits.bytes.size < 32) Future.successful(Right(node))
        else db.put(node).map(Left.apply)
      )
    }
  }

  implicit class NodeEntryOps(val entry: NodeEntry) extends AnyVal {
    def node(implicit db: EvmTrieDb): Future[Node] = entry match {
      case Left(hash) => db.get(hash)
      case Right(node) => Future.successful(node)
    }
  }

  case class Key(leaf: Boolean, nibbles: List[Nibble])

//  sealed trait NodeKey {
//    def apply(nibbles: Iterator[Nibble]): Option[]
//  }


  sealed abstract class Node {
    def get(remainder: Iterator[Nibble])(implicit db: EvmTrieDb): Future[Option[B]]
    def put(remainder: BufferedIterator[Nibble], value: B)(implicit db: EvmTrieDb): Future[Node]
  }
  object Node {
    // Creates branch node for single nibble prefix or extension node for multi-nibble prefix
    def fromPrefix(prefix: Nibbles, entry: NodeEntry): Node = {
      if (prefix.tail.isEmpty) BranchNode.empty.updated(prefix.head, entry)
      else ExtensionNode(prefix, entry)
    }
  }
  sealed abstract class KeyedNode extends Node {
    val key: Nibbles

    def splitKey(prefix: BufferedIterator[Nibble]): (Nibbles, Nibbles) = key.span { nibble =>
      if (prefix.hasNext && (prefix.head == nibble)) {
        prefix.next()
        true
      } else false
    }
  }

  case object EmptyNode extends Node {
    override def get(remainder: Iterator[Nibble])(implicit db: EvmTrieDb) = Future.successful(None)
  }

  case class LeafNode(key: Nibbles, value: B) extends KeyedNode {
    override def get(remainder: Iterator[Nibble])(implicit db: EvmTrieDb): Future[Option[B]] = {
      if (key.forall(n => remainder.hasNext && (n == remainder.next())) && remainder.isEmpty)
        Future.successful(Some(value))
      else Future.successful(None)
    }

    override def put(remainder: BufferedIterator[Nibble], value: B)(implicit db: EvmTrieDb): Future[Node] = {

      val (prefix, suffix) = splitKey(remainder)

      if (suffix.isEmpty && remainder.isEmpty) {
        // Full key match - replace this leaf node
        Future.successful(copy(value = value))

      } else if (prefix.isEmpty && remainder.isEmpty) {
        // No match and incoming key (remainder) was empty - create terminator branch node with 1 leaf node
        NodeEntry(copy(key = this.key.tail)).map(leaf => BranchNode(value).updated(this.key.head, leaf))

      } else if (prefix.isEmpty) {
        // No match and both keys are non-empty - create branch node with 2 leaf nodes
        val head = remainder.next()
        val tail = remainder.toList
        BranchNode(key.head -> copy(key = key.tail), head -> LeafNode(tail, value))

      } else if (remainder.isEmpty) {
        // This leaf's key starts with incoming key (remainder)
        // Note that suffix should be non-empty by this point due to previous conditions

        for {
          leaf <- NodeEntry(copy(key = suffix.tail))
          term <- NodeEntry(BranchNode(value).updated(suffix.head, leaf))
        } yield Node.fromPrefix(prefix, term)

      } else if (suffix.isEmpty) {
        // Incoming key (remainder) starts with this leaf's key
        // Note that remainder should be non-empty by this point due to previous conditions
        val head = remainder.next()
        val tail = remainder.toList
        for {
          leaf <- NodeEntry(LeafNode(tail, value))
          term <- NodeEntry(BranchNode(this.value).updated(head, leaf))
        } yield Node.fromPrefix(prefix, term)

      } else {
        // This node's key and incoming key intersect (share a common, but not complete, prefix)
        // Note that both suffix and remainder should be non-empty by this point due to previous conditions
        val head = remainder.next()
        val tail = remainder.toList
        for {
          branch <- BranchNode(suffix.head -> copy(key = suffix.tail), head -> LeafNode(tail, value))
          entry <- NodeEntry(branch)
        } yield Node.fromPrefix(prefix, entry)
      }
    }
  }

  case class ExtensionNode(key: Nibbles, entry: NodeEntry) extends KeyedNode {
    require(key.size > 1, s"invalid extension node key size (size: ${key.size})")

    override def get(remainder: Iterator[Nibble])(implicit db: EvmTrieDb): Future[Option[B]] = {
      if (key.forall(n => remainder.hasNext && (n == remainder.next())) && remainder.hasNext)
        entry.node.flatMap(_.get(remainder))
      else Future.successful(None)
    }

    override def put(remainder: BufferedIterator[Nibble], value: B)(implicit db: EvmTrieDb): Future[Node] = {
      val (prefix, suffix) = splitKey(remainder)

      if (suffix.isEmpty) {
        // Incoming key (remainder) either starts with or is equal to this node's key - defer to this node's entry
        for {
          node <- entry.node
          updated <- node.put(remainder, value)
          entry <- NodeEntry(updated)
        } yield copy(entry = entry)

      } else if (prefix.isEmpty && remainder.isEmpty) {
        // Incoming key (remainder) is empty - create terminator branch node
        for (entry <- NodeEntry(Node.fromPrefix(key.tail, this.entry)))
          yield BranchNode(value).updated(key.head, entry)

      } else if (prefix.isEmpty) {
        // No match - create branch node
        val head = remainder.next()
        val tail = remainder.toList
        // Create branch -> (extension, leaf) or branch -> (branch, leaf)
        BranchNode(key.head -> Node.fromPrefix(key.tail, this.entry), head -> LeafNode(tail, value))

      } else if (remainder.isEmpty) {
        // This node's key either starts with or is equal to incoming key (remainder)
        // (branch | extension) -> branch(value)(branch | extension)
        for {
          inner <- NodeEntry(Node.fromPrefix(suffix.tail, this.entry))
          entry <- NodeEntry(BranchNode(value).updated(suffix.head, inner))
        } yield Node.fromPrefix(prefix, entry)

      } else {
        // This node's key and incoming key intersect (share a common, but not complete, prefix)
        // (branch | extension) -> branch(leaf, (leaf | branch | extension))
        val head = remainder.next()
        val tail = remainder.toList
        for {
          inner1 <- NodeEntry(Node.fromPrefix(suffix.tail, this.entry))
        } yield blah

      }

    }
  }

  case class BranchNode(entries: List[NodeEntry], value: Option[B]) extends Node {
    require(entries.size == 16, s"invalid entries size (size: ${entries.size})")

    override def get(remainder: Iterator[Nibble])(implicit db: EvmTrieDb): Future[Option[B]] = {
      if (remainder.hasNext) entries(remainder.next()).node.flatMap(_.get(remainder))
      else Future.successful(value)
    }
    def updated(index: Nibble, entry: NodeEntry): BranchNode = copy(entries = entries.updated(index, entry))
  }
  object BranchNode {
    val empty = BranchNode(List.fill(16)(NodeEntry.empty), None)

    def apply(value: B): BranchNode = empty.copy(value = Some(value))

    def apply(nodes: (Nibble, Node)*): Future[BranchNode] = {
      nodes.foldLeft(Future.successful(Array.fill[NodeEntry](16)(NodeEntry.empty))) {
        case (entries, (index, node)) =>
          entries.flatMap(entries => NodeEntry(node).map { entry =>
            entries(index) = entry
            entries
          })
      }.map(entries => BranchNode(entries.toList, None))
    }

  }


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