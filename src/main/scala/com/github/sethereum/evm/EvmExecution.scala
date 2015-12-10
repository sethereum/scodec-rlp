package com.github.sethereum.evm

import scala.collection.immutable.SortedMap
import scala.util.Try


case class EvmExecution(
    running: Boolean = true,
    pc: Int = 0,
    code: Seq[Byte] = Seq(EvmOp.STOP.opcode.toByte))
  extends EvmExecutionOps[EvmExecution]
{

  // TODO: Validate program counter

//  def currentOp: EvmOp = code(pc)

  override def stop: Try[EvmExecution] = Try(copy(running = false))
}

trait EvmExecutionOps[T <: EvmExecutionOps[T]] { this: T =>
  def stop: Try[T]
}

