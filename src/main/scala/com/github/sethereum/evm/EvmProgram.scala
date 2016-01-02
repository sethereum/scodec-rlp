package com.github.sethereum.evm

import com.github.sethereum.evm.EvmOp.JUMPDEST

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}


/**
 * EVM program as a sequence of bytes (bytecode) and the set of valid jump destinations.
 *
 * @param code program bytecode
 * @param jumpDests set of valid jump destinations
 */
case class EvmProgram private (code: IndexedSeq[Byte], jumpDests: Set[Int])

object EvmProgram {

  val STOP = EvmProgram(Seq(EvmOp.STOP))

  def apply(code: IndexedSeq[Byte]): Try[EvmProgram] = {

    @tailrec
    def decode(code: Seq[Byte], i: Int, jumpDests: Set[Int]): Try[Set[Int]] = {
      EvmOp.decode(code) match {
        case Success((op @ JUMPDEST, tail)) => decode(tail, i + op.size, jumpDests + i)
        case Success((op, tail)) => decode(tail, i + op.size, jumpDests)
        case Failure(t) => Failure(new EvmDecodeException(s"decodeOp($i): ${t.getMessage}"))
      }
    }

    decode(code, 0, Set.empty[Int]).map(jd => EvmProgram(code, jd))
  }

  def apply(ops: Seq[EvmOp]): EvmProgram = {
    val codeSize = ops.foldLeft(0)(_ + _.size)
    val buf = ArrayBuffer.empty[Byte]
    buf.sizeHint(codeSize)
    val (code, jumpDests) = ops.foldLeft((buf, Set.empty[Int])) {
      case ((code, jumpDests), op @ JUMPDEST) => (code ++ op.code, jumpDests + code.size)
      case ((code, jumpDests), op) => (code ++ op.code, jumpDests)
    }
    EvmProgram(code, jumpDests)
  }

}