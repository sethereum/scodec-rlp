package com.github.sethereum.evm

import scodec.Attempt.Successful
import scodec.{DecodeResult, Attempt, Codec}
import scodec.bits.BitVector

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.{ArrayBuffer, Builder}
import scala.collection.{IndexedSeqLike, LinearSeq, LinearSeqOptimized}
import scala.util.Try

import scodec.codecs._

//final class EvmOpSeq private (val code: Seq[Byte])
//  extends LinearSeq[EvmOp] with LinearSeqOptimized[EvmOp, EvmOpSeq] {
//
//  val headTailOpt: Option[(EvmOp, EvmOpSeq)] = code match {
//    case EvmOp(op, tail) => Some((op, EvmOpSeq.fromCode(tail)))
//    case _ => None
//  }
//
//  override protected[this] def newBuilder: Builder[EvmOp, EvmOpSeq] = EvmOpSeq.newBuilder
//
//  override def isEmpty: Boolean = code.isEmpty
//
//  override def tail: EvmOpSeq = super.tail
//
//  override def head: EvmOp = super.head
//}
//
//object EvmOpSeq {
//
//  def fromSeq(ops: Seq[EvmOp]): EvmOpSeq = {
//    val size = ops.foldLeft(0)(_ + _.size)
//    val buf = ArrayBuffer[Byte]()
//    buf.sizeHint(size)
//    val code = ops.foldLeft(buf)(_ ++= _.code)
//    fromCode(code)
//  }
//
//  def fromCode(code: Seq[Byte]) = new EvmOpSeq(code)
//
//  def apply(code: Byte*) = fromCode(code)
//
//  def newBuilder: Builder[EvmOp, EvmOpSeq] =
//    new ArrayBuffer mapResult fromSeq
//
//
//}



final class EvmProgram private (private val ops: IndexedSeq[EvmOp], val jumps: Map[Int, Int])
  extends IndexedSeq[EvmOp] with IndexedSeqLike[EvmOp, EvmProgram] {

  override protected[this] def newBuilder: Builder[EvmOp, EvmProgram] = EvmProgram.newBuilder

  override def length: Int = ops.length

  override def apply(idx: Int): EvmOp = ops.apply(idx)
}

object EvmProgram {

  import EvmOp._

  def fromOps(ops: IndexedSeq[EvmOp]): EvmProgram = {
    // Build the jump dest map by traversing the operations
    val (codeSize, opsSize, jumps) = ops.foldLeft((0, 0, Map.empty[Int, Int])) {
      case ((codeSize, opsSize, jumpDests), op) =>
        (codeSize + op.size, opsSize + 1, op match {
          case JUMPDEST => jumpDests + (codeSize -> opsSize)
          case _ => jumpDests
        })
    }
    new EvmProgram(ops, jumps)
  }

//  def fromCode(code: Byte*): Try[EvmProgram] = {
//    EvmOp.codec.decode(BitVector(code))
//  }

  def apply(ops: IndexedSeq[EvmOp]) = fromOps(ops)

  def newBuilder: Builder[EvmOp, EvmProgram] =
    new ArrayBuffer mapResult fromOps

  implicit def canBuildFrom: CanBuildFrom[EvmProgram, EvmOp, EvmProgram] =
    new CanBuildFrom[EvmProgram, EvmOp, EvmProgram] {
      def apply(): Builder[EvmOp, EvmProgram] = newBuilder
      def apply(from: EvmProgram): Builder[EvmOp, EvmProgram] = newBuilder
    }


  val codec: Codec[EvmProgram] = {

    def encode(program: EvmProgram): Attempt[BitVector] = {
      val ops = program.ops
      val size = ops.foldLeft(0)(_ + _.size)
      val buf = ArrayBuffer[Byte]()
      buf.sizeHint(size)
      val code = ops.foldLeft(buf)(_ ++ _.code)

      Successful(BitVector(code))
    }

    def decode(bits: BitVector): Attempt[DecodeResult[EvmProgram]] = for {
      ops <- list(EvmOp.codec).decode(bits)
    } yield ops.map(list => EvmProgram.fromOps(list.toIndexedSeq))

    Codec(encode _, decode _)
  }
}