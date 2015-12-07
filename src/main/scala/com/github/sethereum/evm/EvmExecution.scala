package com.github.sethereum.evm

import scala.collection.immutable.SortedMap
import scala.util.Try


case class EvmExecution(
    running: Boolean = true,
    programCounter: Int = 0,
    code: List[EvmOp] = List(EvmOp.STOP))
  extends EvmExecutionOps[EvmExecution]
{

  // TODO: Validate program counter

  def currentOp: EvmOp = code(programCounter)

  override def stop: Try[EvmExecution] = Try(copy(running = false))
}

trait EvmExecutionOps[T <: EvmExecutionOps[T]] { this: T =>
  def stop: Try[T]
}

