package com.github.sethereum.evm

import scala.util.Try


case class EvmExecution private (
    running: Boolean = true,
    pc: Int = 0,
    code: Seq[Byte] = Seq(EvmOp.STOP.opcode.toByte),
    program: EvmProgram = EvmProgram(List(EvmOp.STOP).toIndexedSeq))
  extends EvmExecutionOps[EvmExecution]
{

  // TODO: Validate program counter

//  def this(code: Seq[Byte]) = this(code = code, jumpDestinations = jumpDestinations(code))

//  private def jumpDestinations(code: Seq[Byte]): Seq[Int] = Seq.empty

  override def stop: Try[EvmExecution] = Try(copy(running = false))

//  override def jump(pc: Int): Try[EvmExecution] = Try(
//    if (jumpDestinations(pc)) copy(pc = pc)
//    else throw new EvmException(s"invalid jump destination $pc")
//  )
}

object EvmExecution {

  def apply(): EvmExecution = new EvmExecution()

//  def apply(code: Seq[Byte]): EvmExecution = new EvmExecution(code)
}

trait EvmExecutionOps[T <: EvmExecutionOps[T]] { this: T =>
  def stop: Try[T]
//  def jump(pc: Int): Try[T]
}

