package com.github.sethereum.evm

import com.github.sethereum.evm.EvmOp.JUMPDEST

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}


/**
 * Represents a valid EVM bytecode sequence and the set of valid jump destinations it contains.
 *
 * Instances of this class can only be instantiated with valid bytecode sequences. Note that a program's bytecode is
 * decoded during construction to enforce this constraint.
 *
 * @param code program bytecode
 * @param jumpDests set of valid jump destinations
 */
case class EvmProgram private (code: Seq[Byte], jumpDests: Set[Int])

object EvmProgram {

  val STOP = EvmProgram.encode(Seq(EvmOp.STOP))

  /**
   * Create a program from a valid bytecode sequence.
   *
   * @param code bytecode sequence, which SHOULD be an IndexedSeq for performant random access (for JUMP)
   * @return Success containing decoded program if given sequence is valid, Failure with decode exception otherwise
   */
  def apply(code: Seq[Byte]): Try[EvmProgram] = {

    @tailrec
    def decode(code: Seq[Byte], i: Int, jumpDests: Set[Int]): Try[Set[Int]] = {
      EvmOp.decode(code) match {
        case Success((op @ JUMPDEST, tail)) => decode(tail, i + op.size, jumpDests + i)
        case Success((op, tail)) => decode(tail, i + op.size, jumpDests)
        case Failure(t) => Failure(new EvmDecodeException(s"decode($i): ${t.getMessage}"))
      }
    }

    decode(code.view, 0, Set.empty[Int]).map(jd => EvmProgram(code.view, jd))
  }

  def encode(ops: Seq[EvmOp]): EvmProgram = {
    val codeSize = ops.foldLeft(0)(_ + _.size)
    val buf = ArrayBuffer.empty[Byte]
    buf.sizeHint(codeSize)
    val (code, jumpDests) = ops.foldLeft((buf, Set.empty[Int])) {
      case ((code, jumpDests), op @ JUMPDEST) => (code ++ op.code, jumpDests + code.size)
      case ((code, jumpDests), op) => (code ++ op.code, jumpDests)
    }
    EvmProgram(code.view, jumpDests)
  }

}